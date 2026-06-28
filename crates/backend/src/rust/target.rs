use super::{rir::RirCtxPlan, syntax};

const RT: &str = "anvyx_runtime";

fn rt_path(item: &str) -> String {
    format!("{RT}::{item}")
}

pub(super) fn anv_string_ty() -> String {
    rt_path("AnvString")
}

pub(super) fn anv_list_ty(elem: &str) -> String {
    format!("{}<'cx, {elem}>", rt_path("AnvList"))
}

pub(super) fn list_storage_ty(elem: &str) -> String {
    format!("{}<'cx, {elem}>", rt_path("ListStorage"))
}

pub(super) fn anv_slice_ty(elem: &str) -> String {
    format!("{}<'cx, {elem}>", rt_path("AnvSlice"))
}

pub(super) fn anv_map_ty(key: &str, value: &str) -> String {
    format!("{}<'cx, {key}, {value}>", rt_path("AnvMap"))
}

pub(super) fn anv_ref_alias(alias: &str, native: &str) -> String {
    format!("type {alias}<'cx> = {}<'cx, {native}>;", rt_path("AnvRef"))
}

pub(super) fn native_ref_borrow(resource: &str, arg: &str, mutable: bool, body: &str) -> String {
    let access = if mutable { "with_mut" } else { "with" };
    let heap = if mutable {
        format!("{}.heap()", runtime_param_name())
    } else {
        format!("{}.heap_ref()", runtime_param_name())
    };
    format!(
        "({resource}).{access}({heap}, |{arg}| -> Result<_, {}> {{ Ok({body}) }}).map_err({})??",
        runtime_error_ty(),
        heap_access_error(),
    )
}

pub(super) fn native_ref_adopt(native: &str, value: &str) -> String {
    format!(
        "{{ let __anv_native_ret = {value}; {}::<{native}>::register_untracked({}.heap()).alloc_in({}, __anv_native_ret) }}",
        rt_path("AnvRefType"),
        runtime_param_name(),
        runtime_param_name(),
    )
}

pub(super) fn init_field_provided(value: &str) -> String {
    format!("{}::Provided({value})", rt_path("AnvInitField"))
}

pub(super) fn init_field_omitted() -> String {
    format!("{}::Omitted", rt_path("AnvInitField"))
}

pub(super) fn map_storage_ty(key: &str, value: &str) -> String {
    format!("{}<'cx, {key}, {value}>", rt_path("MapStorage"))
}

pub(super) fn stack_lambda_cell_ty(payload: &str) -> String {
    format!("{}<{payload}>", rt_path("StackLambdaCell"))
}

pub(super) fn lambda_cell_ty(payload: &str) -> String {
    format!("{}<{payload}>", rt_path("LambdaCell"))
}

pub(super) fn scoped_lambda_ty(args: &str, ret: &str) -> String {
    format!("{}<'_, 'cx, {args}, {ret}>", rt_path("ScopedLambda"))
}

pub(super) fn scoped_lambda_ctor(args: &str, ret: &str) -> String {
    format!("{}::<'_, 'cx, {args}, {ret}>", rt_path("ScopedLambda"))
}

pub(super) fn scoped_lambda_thunk() -> &'static str {
    "__anv_scoped_call"
}

pub(super) fn generated_runtime_symbol() -> &'static str {
    "AnvRuntime"
}

pub(super) fn generated_runtime_inner_symbol() -> &'static str {
    "AnvRuntimeInner"
}

pub(super) fn generated_callback_registry_symbol() -> &'static str {
    "AnvCallbackRegistry"
}

pub(super) fn callback_record_symbol(sig: usize) -> String {
    format!("CallbackRecordSig{sig}")
}

pub(super) fn callback_table_field(sig: usize) -> String {
    format!("sig{sig}")
}

pub(super) fn callback_record_heap_type_field(sig: usize) -> String {
    format!("callback_record_sig{sig}")
}

pub(super) fn callback_call_thunk_symbol(sig: usize) -> String {
    format!("__anv_callback_call_sig{sig}")
}

pub(super) fn callback_close_thunk_symbol(sig: usize) -> String {
    format!("__anv_callback_close_sig{sig}")
}

pub(super) fn escaping_lambda_ty(args: &str, ret: &str) -> String {
    format!("{}<{args}, {ret}>", rt_path("EscapingLambda"))
}

pub(super) fn escaping_lambda_ctor_ty(args: &str, ret: &str) -> String {
    format!("{}::<{args}, {ret}>", rt_path("EscapingLambda"))
}

pub(super) fn callback_key_ty() -> String {
    rt_path("CallbackKey")
}

pub(super) fn callback_check_identity(key: &str, table_id: usize, signature_id: usize) -> String {
    format!(
        "{key}.__anvyx_check_identity(std::num::NonZeroU64::new({table_id}).unwrap(), std::num::NonZeroU64::new({signature_id}).unwrap())"
    )
}

pub(super) fn callback_slot_ty(root: &str) -> String {
    format!("{}<{root}>", rt_path("CallbackSlot"))
}

pub(super) fn callback_slot_turbofish(root: &str) -> String {
    format!("{}::<{root}>", rt_path("CallbackSlot"))
}

pub(super) fn root_id_ty(payload: &str) -> String {
    format!("{}<'cx, {payload}>", rt_path("RootId"))
}

pub(super) fn pin_box_ty(payload: &str) -> String {
    format!("std::pin::Pin<Box<{payload}>>")
}

pub(super) fn box_pin_struct_start(ty: &str) -> String {
    format!("Box::pin({ty} {{")
}

pub(super) fn phantom_pinned_ty() -> &'static str {
    "std::marker::PhantomPinned"
}

pub(super) fn phantom_pinned_value() -> &'static str {
    "std::marker::PhantomPinned"
}

pub(super) fn runtime_owner_handle_ty() -> String {
    rt_path("RuntimeOwnerHandle")
}

pub(super) fn runtime_owner_handle_new() -> String {
    format!("{}::new()", runtime_owner_handle_ty())
}

pub(super) fn owner_attach(owner: &str, ptr: &str) -> String {
    format!("{owner}.__anvyx_attach_owner_ptr({ptr})")
}

pub(super) fn owner_begin_shutdown(owner: &str) -> String {
    format!("{owner}.__anvyx_begin_shutdown()")
}

pub(super) fn owner_enter_current(owner: &str) -> String {
    format!("{owner}.__anvyx_enter_current()")
}

pub(super) fn owner_enter(owner: &str, owner_id: &str, shutdown_generation: &str) -> String {
    format!("{owner}.__anvyx_enter({owner_id}, {shutdown_generation})")
}

pub(super) fn owner_suspend_for_provider(owner: &str) -> String {
    format!("{owner}.__anvyx_suspend_entry_for_provider()")
}

pub(super) fn owner_entry_ptr(entry: &str) -> String {
    format!("{entry}.owner_ptr()")
}

pub(super) fn non_null_cast_mut(ptr: &str, ty: &str) -> String {
    format!("{ptr}.cast::<{ty}>().as_mut()")
}

pub(super) fn pin_get_unchecked_mut(value: &str) -> String {
    format!("{value}.get_unchecked_mut()")
}

pub(super) fn non_null_unit_ty() -> &'static str {
    "std::ptr::NonNull<()>"
}

pub(super) fn non_null_ty(payload: &str) -> String {
    format!("std::ptr::NonNull<{payload}>")
}

pub(super) fn non_null_from_mut(value: &str) -> String {
    format!("std::ptr::NonNull::from({value})")
}

pub(super) fn non_null_from_ref(value: &str) -> String {
    format!("std::ptr::NonNull::from({value})")
}

pub(super) fn mut_place_ty() -> String {
    rt_path("MutPlace")
}

pub(super) enum DataRefPlaceOp {
    Access,
    Mutate,
}

impl DataRefPlaceOp {
    pub(super) fn name(&self) -> &'static str {
        match self {
            Self::Access => "access",
            Self::Mutate => "mutate",
        }
    }

    pub(super) fn payload_ref(&self, payload: &str) -> String {
        match self {
            Self::Access => format!("&{payload}"),
            Self::Mutate => format!("&mut {payload}"),
        }
    }

    pub(super) fn path_ref(&self) -> &'static str {
        match self {
            Self::Access => "&",
            Self::Mutate => "&mut ",
        }
    }

    pub(super) fn heap_access(
        &self,
        rt: &str,
        object: &str,
        heap_type: &str,
        storage: &str,
        storage_ty: &str,
        body: &str,
    ) -> String {
        match self {
            Self::Access => {
                rt_heap_try_with_erased(rt, object, heap_type, storage, storage_ty, body)
            }
            Self::Mutate => {
                rt_heap_try_with_erased_mut(rt, object, heap_type, storage, storage_ty, body)
            }
        }
    }
}

pub(super) fn dataref_place_ops_ty(payload: &str) -> String {
    format!("{}<'cx, {payload}>", rt_path("DataRefPlaceOps"))
}

pub(super) fn projection_ops_ty(root: &str, payload: &str) -> String {
    format!("{}<'cx, {root}, {payload}>", rt_path("ProjectionOps"))
}

pub(super) fn optional_payload_ops_ty(payload: &str) -> String {
    format!("{}<{payload}>", rt_path("OptionalPayloadOps"))
}

pub(super) fn optional_payload_ops_ctor(payload: &str) -> String {
    format!("{}::<{payload}>::default()", rt_path("OptionalPayloadOps"))
}

pub(super) fn map_value_ops_ty(key: &str) -> String {
    format!("{}<{key}>", rt_path("MapValueOps"))
}

pub(super) fn map_value_ops_ctor(key: &str, loan: &str) -> String {
    format!("{}::new({key}, &{loan})", rt_path("MapValueOps"))
}

pub(super) fn erased_handle_ty() -> String {
    format!("{}<'cx>", rt_path("ErasedHandle"))
}

pub(super) fn scoped_mut_place_cell_ty(source_lifetime: &str, payload: &str) -> String {
    format!(
        "{}<{source_lifetime}, 'cx, {payload}>",
        rt_path("ScopedMutPlaceCell")
    )
}

pub(super) fn stack_lambda_cell_ctor(payload: &str) -> String {
    format!("{}::<{payload}>", rt_path("StackLambdaCell"))
}

pub(super) fn lambda_cell_ctor(payload: &str) -> String {
    format!("{}::<{payload}>", rt_path("LambdaCell"))
}

pub(super) fn lambda_cell_set(value: &str, replace_collection: bool) -> String {
    if replace_collection {
        format!("mutate(|slot| slot.replace_with({value}))")
    } else {
        format!("set({value})")
    }
}

pub(super) fn scoped_mut_place_cell_new(source: &str) -> String {
    format!("{}::new({source})", rt_path("ScopedMutPlaceCell"))
}

pub(super) fn runtime_error_ty() -> String {
    rt_path("RuntimeError")
}

pub(super) fn result_ty(ret: &str) -> String {
    format!("Result<{ret}, {}>", runtime_error_ty())
}

pub(super) fn generated_types_symbol(ctx: &RirCtxPlan) -> &str {
    ctx.types_symbol.as_str()
}

pub(super) fn generated_globals_symbol(ctx: &RirCtxPlan) -> &str {
    ctx.globals_symbol.as_str()
}

pub(super) fn runtime_param_name() -> &'static str {
    "rt"
}

pub(super) fn types_param_name() -> &'static str {
    "types"
}

pub(super) fn globals_param_name() -> &'static str {
    "globals"
}

pub(super) fn owner_param_name() -> &'static str {
    "owner"
}

pub(super) fn callbacks_param_name() -> &'static str {
    "callbacks"
}

pub(super) fn runtime_param(used: bool) -> &'static str {
    if used { "rt" } else { "_rt" }
}

pub(super) fn types_param(used: bool) -> &'static str {
    if used { "types" } else { "_types" }
}

pub(super) fn globals_param(used: bool) -> &'static str {
    if used { "globals" } else { "_globals" }
}

pub(super) fn runtime_ctx_ty() -> String {
    runtime_ctx_ty_with("'rt")
}

pub(super) fn runtime_ctx_ref_ty() -> String {
    format!("&mut {}", runtime_ctx_ty())
}

pub(super) fn runtime_ctx_ty_with(rt_lifetime: &str) -> String {
    format!("{}<'cx, {rt_lifetime}>", rt_path("Ctx"))
}

pub(super) fn runtime_ctx_from_raw(heap: &str) -> String {
    format!("unsafe {{ {}::__anvyx_from_raw({heap}) }}", rt_path("Ctx"))
}

pub(super) fn runtime_ctx_from_raw_with_roots(heap: &str, roots: &str) -> String {
    format!(
        "unsafe {{ {}::__anvyx_from_raw_with_roots({heap}, {roots}) }}",
        rt_path("Ctx")
    )
}

pub(super) fn types_ref_ty(symbol: &str) -> String {
    format!("&{symbol}<'cx>")
}

pub(super) fn globals_ref_ty(symbol: &str) -> String {
    format!("&{symbol}<'cx>")
}

pub(super) fn global_slot_ty(payload: &str) -> String {
    format!("{}<{payload}>", rt_path("GlobalSlot"))
}

pub(super) fn global_slot_new(name: &str) -> String {
    format!("{}::new({name:?})", rt_path("GlobalSlot"))
}

pub(super) fn global_slot_field(globals: &str, field: &str) -> String {
    format!("{globals}.{field}")
}

pub(super) fn global_ensure(slot: &str, init: &str) -> String {
    format!("{slot}.ensure(|| {init})?")
}

pub(super) fn global_read(slot: &str, init: &str) -> String {
    format!("{slot}.read(|| {init})?")
}

pub(super) fn global_write(slot: &str, init: &str) -> String {
    format!("{slot}.write(|| {init})?")
}

pub(super) fn global_set_without_init(slot: &str, value: &str) -> String {
    format!("{slot}.set_without_init({value})?")
}

pub(super) fn global_set_or_replace_collection(slot: &str, value: &str) -> String {
    format!("{slot}.set_without_init_or_replace({value}, |slot, value| slot.replace_with(value))?")
}

pub(super) fn global_begin_projected_loan(slot: &str) -> String {
    format!("{slot}.begin_projected_loan()")
}

pub(super) fn generated_call_args(args: impl IntoIterator<Item = String>) -> Vec<String> {
    generated_call_args_inner(false, args)
}

pub(super) fn retained_generated_call_args(args: impl IntoIterator<Item = String>) -> Vec<String> {
    generated_call_args_inner(true, args)
}

fn generated_call_args_inner(
    retained_callbacks: bool,
    args: impl IntoIterator<Item = String>,
) -> Vec<String> {
    let mut context = vec![
        runtime_param_name().to_string(),
        types_param_name().to_string(),
        globals_param_name().to_string(),
    ];
    if retained_callbacks {
        context.extend([
            owner_param_name().to_string(),
            callbacks_param_name().to_string(),
        ]);
    }
    context.into_iter().chain(args).collect()
}

pub(super) fn generated_call(symbol: &str, args: impl IntoIterator<Item = String>) -> String {
    format!("{symbol}({})", syntax::comma(generated_call_args(args)))
}

pub(super) fn retained_generated_call(
    symbol: &str,
    args: impl IntoIterator<Item = String>,
) -> String {
    format!(
        "{symbol}({})",
        syntax::comma(retained_generated_call_args(args))
    )
}

pub(super) fn native_call_args(args: impl IntoIterator<Item = String>) -> Vec<String> {
    std::iter::once(runtime_param_name().to_string())
        .chain(args)
        .collect()
}

pub(super) fn heap_ty() -> String {
    format!("{}<'cx>", rt_path("Heap"))
}

pub(super) fn heap_type_ty(storage: &str) -> String {
    format!("{}<'cx, {storage}>", rt_path("HeapType"))
}

pub(super) fn handle_ty(storage: &str) -> String {
    format!("{}<'cx, {storage}>", rt_path("Handle"))
}

pub(super) fn trace_ty() -> String {
    rt_path("Trace")
}

pub(super) fn trace_driver_ty() -> String {
    rt_path("TraceDriver")
}

pub(super) fn ctx_roots_ty() -> String {
    rt_path("CtxRoots")
}

pub(super) fn visitor_ty(driver: &str) -> String {
    format!("{}<'cx, '_, {driver}>", rt_path("Visitor"))
}

pub(super) fn trace_derive(extra: &[&str]) -> String {
    let mut traits = extra
        .iter()
        .map(|item| (*item).to_string())
        .collect::<Vec<_>>();
    traits.push(trace_ty());
    format!("#[derive({})]", syntax::comma(traits))
}

pub(super) fn trace_crate_attr(cx_dependent: bool) -> String {
    if cx_dependent {
        format!("#[trace(crate = {RT}, ctx = 'cx)]")
    } else {
        format!("#[trace(crate = {RT})]")
    }
}

pub(super) fn trace_impl_header(impl_generics: &str, ty: &str) -> String {
    format!("unsafe impl{impl_generics} {}<'cx> for {ty}", trace_ty())
}

pub(super) fn trace_fn_header() -> String {
    format!(
        "fn trace<D: {}<'cx>>(&self, visitor: &mut {})",
        trace_driver_ty(),
        visitor_ty("D")
    )
}

pub(super) fn anv_string_from(expr: &str) -> String {
    format!("{}::from({expr})", anv_string_ty())
}

pub(super) fn rust_option_map(option: &str, body: &str) -> String {
    format!("({option}).map(|value| {body})")
}

pub(super) fn rust_array_map(array: &str, body: &str) -> String {
    format!("({array}).map(|value| {{ {body} }})")
}

pub(super) fn rust_result_match(value: &str, ok: &str, err: &str) -> String {
    format!("match {value} {{ {ok}, {err} }}")
}

pub(super) fn rust_ok(value: &str) -> String {
    format!("Ok({value})")
}

pub(super) fn rust_err(value: &str) -> String {
    format!("Err({value})")
}

pub(super) fn rust_tuple(fields: impl IntoIterator<Item = String>) -> String {
    let fields = fields.into_iter().collect::<Vec<_>>();
    match fields.as_slice() {
        [] => rust_unit(),
        [field] => format!("({field},)"),
        _ => format!("({})", fields.join(", ")),
    }
}

pub(super) fn rust_unit() -> String {
    "()".to_string()
}

pub(super) fn rust_eval_then(eval: &str, tail: &str) -> String {
    format!("{{ let _ = {eval}; {tail} }}")
}

pub(super) fn rust_tuple_field(value: &str, index: usize) -> String {
    format!("{value}.{index}")
}

pub(super) fn anv_string_format(fmt: &str, arg: &str) -> String {
    anv_string_from(&format!("format!({fmt}, {arg})"))
}

pub(super) fn display_float(value: &str) -> String {
    rt_path(&format!("display_float({value})"))
}

pub(super) fn anv_list_from_elems(rt: &str, storage_ty: &str, elems: &str) -> String {
    format!(
        "{}::from_elems({rt}, {storage_ty}, [{elems}])",
        rt_path("AnvList")
    )
}

pub(super) fn anv_list_from_iter(rt: &str, storage_ty: &str, iter: &str) -> String {
    format!(
        "{}::from_elems({rt}, {storage_ty}, {iter})",
        rt_path("AnvList")
    )
}

pub(super) fn anv_slice_from_raw_parts(
    ptr: &str,
    root_len: &str,
    start: &str,
    len: &str,
) -> String {
    format!(
        "unsafe {{ {}::from_raw_parts({ptr}, {root_len}, {start}, {len}) }}",
        rt_path("AnvSlice")
    )
}

pub(super) fn anv_slice_from_raw_parts_mut(
    ptr: &str,
    root_len: &str,
    start: &str,
    len: &str,
) -> String {
    format!(
        "unsafe {{ {}::from_raw_parts_mut({ptr}, {root_len}, {start}, {len}) }}",
        rt_path("AnvSlice")
    )
}

pub(super) fn anv_slice_from_list(root: &str, start: &str, len: &str) -> String {
    format!(
        "{}::from_list({root}, {start}, {len})?",
        rt_path("AnvSlice")
    )
}

pub(super) fn anv_slice_from_list_mut(rt: &str, root: &str, start: &str, len: &str) -> String {
    format!(
        "{}::from_list_mut({rt}, {root}, {start}, {len})?",
        rt_path("AnvSlice")
    )
}

pub(super) fn anv_slice_slice(source: &str, start: &str, len: &str) -> String {
    format!("{source}.slice({start}, {len})")
}

pub(super) fn anv_slice_copy_range_with(
    source: &str,
    rt: &str,
    storage_ty: &str,
    range: &str,
    item: &str,
    body: &str,
) -> String {
    format!("unsafe {{ {source}.copy_range_with({rt}, {storage_ty}, {range}, |{item}| {body}) }}")
}

pub(super) fn mut_place_slice_view(
    place: &str,
    rt: &str,
    start: &str,
    end: &str,
    inclusive: bool,
    mutable: bool,
    raw: bool,
) -> String {
    let view = if mutable {
        format!("{place}.slice_view_mut({rt}, {start}, {end}, {inclusive})")
    } else {
        format!("{place}.slice_view({start}, {end}, {inclusive})")
    };
    if raw {
        format!("unsafe {{ {view} }}?")
    } else {
        format!("{view}?")
    }
}

pub(super) fn anv_map_from_entries(rt: &str, storage_ty: &str, entries: &str) -> String {
    format!(
        "{}::from_entries({rt}, {storage_ty}, [{entries}])",
        rt_path("AnvMap")
    )
}

pub(super) fn list_push_ctx_region(value: &str) -> String {
    format!("{{ value.push(rt, {value})?; Ok(()) }}")
}

pub(super) fn map_insert_region(key: &str, inserted: &str) -> String {
    format!("{{ value.insert(rt, {key}, {inserted})?; Ok(()) }}")
}

pub(super) fn map_contains_key_region(key: &str) -> String {
    format!("value.contains_key(rt, &{key})")
}

pub(super) fn map_remove_region(key: &str) -> String {
    format!("value.remove(rt, &{key})")
}

pub(super) fn collection_structural_version(collection: &str) -> String {
    format!("{collection}.structural_version()")
}

pub(super) fn list_with_elem_shared_short(
    list: &str,
    rt: &str,
    index: &str,
    version: &str,
    body: &str,
) -> String {
    format!("{list}.with_elem_shared_short({rt}, {index}, {version}, |value| {{ {body} }})")
}

pub(super) fn list_with_elem_owned_mut_short(
    list: &str,
    rt: &str,
    index: &str,
    version: &str,
    body: &str,
) -> String {
    format!(
        "{list}.with_elem_owned_mut_ctx_short({rt}, {index}, {version}, |rt, value| {{ {body} }})"
    )
}

pub(super) fn slice_elem_at_shared(slice: &str, rt: &str, index: &str) -> String {
    format!("{slice}.elem_at_shared({rt}, {index})")
}

pub(super) fn slice_with_elem_owned_mut_short(
    slice: &str,
    rt: &str,
    index: &str,
    body: &str,
) -> String {
    format!("{slice}.with_elem_owned_mut_ctx_short({rt}, {index}, |rt, value| {{ {body} }})")
}

pub(super) fn map_key_at_shared(map: &str, rt: &str, index: &str, version: &str) -> String {
    format!("{map}.key_at_shared({rt}, {index}, {version})?")
}

pub(super) fn map_value_at_shared(map: &str, rt: &str, index: &str, version: &str) -> String {
    format!("{map}.value_at_shared({rt}, {index}, {version})?")
}

pub(super) fn map_with_value_mut_short(
    map: &str,
    rt: &str,
    index: &str,
    version: &str,
    body: &str,
) -> String {
    format!(
        "unsafe {{ {map}.with_value_mut_short({rt}, {index}, {version}, |value| {{ {body} }}) }}"
    )
}

pub(super) fn collection_len(root: &str) -> String {
    format!("({root}).len() as i64")
}

pub(super) fn begin_shape_loan(root: &str) -> String {
    format!("{root}.begin_shape_loan()?")
}

pub(super) fn mut_place_begin_shape_loan(place: &str, runtime: &str) -> String {
    mut_place_access_ctx(
        place,
        runtime,
        &format!("Ok({})", begin_shape_loan("value")),
    )
}

pub(super) fn map_begin_value_loan_region(key: &str) -> String {
    format!("value.begin_value_loan_by_key(rt, &{key})")
}

pub(super) fn shape_loan_version(loan: &str) -> String {
    format!("{loan}.version()")
}

pub(super) fn heap_register(tracked: bool) -> &'static str {
    if tracked {
        "register_tracked"
    } else {
        "register_untracked"
    }
}

pub(super) fn rt_heap_alloc(rt: &str, heap_type: &str, storage: &str) -> String {
    format!("{rt}.heap().alloc({heap_type}, {storage})")
}

pub(super) fn rt_heap_with(rt: &str, object: &str, storage: &str, body: &str) -> String {
    format!("{rt}.heap().with({object}, |{storage}| {body})")
}

pub(super) fn rt_heap_with_mut(rt: &str, object: &str, storage: &str, body: &str) -> String {
    format!("{rt}.heap().with_mut({object}, |{storage}| {{ {body} }})")
}

pub(super) fn heap_access_error() -> String {
    rt_path("heap_access_error")
}

pub(super) fn map_heap_access_error(expr: &str) -> String {
    format!("{expr}.map_err({})?", heap_access_error())
}

pub(super) fn rt_heap_erase(rt: &str, object: &str) -> String {
    map_heap_access_error(&format!("{rt}.heap().erase({object})"))
}

pub(super) fn rt_heap_try_with_erased(
    rt: &str,
    object: &str,
    heap_type: &str,
    storage: &str,
    storage_ty: &str,
    body: &str,
) -> String {
    rt_heap_try_with_erased_op(rt, object, heap_type, storage, storage_ty, body, false)
}

pub(super) fn rt_heap_try_with_erased_mut(
    rt: &str,
    object: &str,
    heap_type: &str,
    storage: &str,
    storage_ty: &str,
    body: &str,
) -> String {
    rt_heap_try_with_erased_op(rt, object, heap_type, storage, storage_ty, body, true)
}

fn rt_heap_try_with_erased_op(
    rt: &str,
    object: &str,
    heap_type: &str,
    storage: &str,
    storage_ty: &str,
    body: &str,
    mutable: bool,
) -> String {
    let method = if mutable {
        "try_with_erased_mut"
    } else {
        "try_with_erased"
    };
    let storage_ref = if mutable { "&mut " } else { "&" };
    map_heap_access_error(&format!(
        "{rt}.heap().{method}({object}, {heap_type}, |{storage}: {storage_ref}{storage_ty}| {body})"
    ))
}

pub(super) fn heap_type_access(types: &str, heap_type: &str) -> String {
    format!("{types}.{heap_type}")
}

pub(super) fn mut_place_local(slot: &str) -> String {
    format!("{}::local(&mut {slot})", mut_place_ty())
}

pub(super) fn mut_place_local_raw(slot: &str) -> String {
    format!(
        "unsafe {{ {}::local_raw(&raw mut {slot}) }}",
        mut_place_ty()
    )
}

pub(super) fn mut_place_reborrow(place: &str) -> String {
    format!("{place}.reborrow()")
}

pub(super) fn mut_place_stack_cell(cell: &str) -> String {
    format!("{}::stack_cell(&{cell})", mut_place_ty())
}

pub(super) fn mut_place_heap_cell(cell: &str) -> String {
    format!("{}::heap_cell({cell}.clone())", mut_place_ty())
}

pub(super) fn mut_place_global(slot: &str, init: &str) -> String {
    format!("{}::global(&{slot}, &|rt| {init})", mut_place_ty())
}

pub(super) fn mut_place_global_with_init(slot: &str, init: &str) -> String {
    format!("{}::global(&{slot}, {init})", mut_place_ty())
}

pub(super) fn mut_place_scoped_cell(cell: &str) -> String {
    format!("{}::scoped_cell({cell})", mut_place_ty())
}

pub(super) fn mut_place_dataref(object: &str, ops: &str) -> String {
    format!("{}::dataref({object}, {ops})", mut_place_ty())
}

pub(super) fn mut_place_projected(root: &str, ops: &str) -> String {
    format!("{}::projected({root}, {ops})", mut_place_ty())
}

pub(super) fn dataref_place_heap_type_field() -> &'static str {
    "heap_type"
}

pub(super) fn dataref_place_heap_type_access(receiver: &str) -> String {
    format!("{receiver}.{}", dataref_place_heap_type_field())
}

pub(super) fn mut_place_set(place: &str, runtime: &str, value: &str) -> String {
    format!("{place}.set({runtime}, {value})?")
}

pub(super) fn mut_place_replace_collection(
    place: &str,
    runtime: &str,
    replacement: &str,
) -> String {
    mut_place_region(
        place,
        "mutate",
        runtime,
        "slot",
        &format!("slot.replace_with({replacement})"),
    )
}

pub(super) fn replace_collection_result(place: &str, value: &str) -> String {
    format!("{place}.replace_with({value})")
}

pub(super) fn replace_collection(place: &str, value: &str) -> String {
    format!("{}?", replace_collection_result(place, value))
}

pub(super) fn mut_place_access(place: &str, runtime: &str, body: &str) -> String {
    mut_place_region(place, "access", runtime, "value", body)
}

pub(super) fn mut_place_access_ctx(place: &str, runtime: &str, body: &str) -> String {
    format!("{place}.access_with_ctx({runtime}, |rt, value| {body})?")
}

pub(super) fn mut_place_mutate_ctx(place: &str, runtime: &str, body: &str) -> String {
    format!("{place}.mutate_with_ctx({runtime}, |rt, value| {body})?")
}

pub(super) fn mut_place_get_copy(place: &str, runtime: &str) -> String {
    format!("{place}.get_copy({runtime})?")
}

fn mut_place_region(place: &str, op: &str, runtime: &str, slot: &str, body: &str) -> String {
    format!("{place}.{op}({runtime}, |{slot}| {body})?")
}

#[cfg(test)]
pub(super) fn heap_scope() -> String {
    rt_path("Heap::scope")
}

pub(super) fn heap_scope_owned() -> String {
    rt_path("Heap::scope_owned")
}

#[cfg(test)]
pub(super) fn checked_index(index: &str, len: &str) -> String {
    format!("{}({index}, {len})", rt_path("checked_index"))
}

pub(super) fn checked_index_result(index: &str, len: &str, kind: &str) -> String {
    format!(
        "{}({index}, {len}, {kind:?})?",
        rt_path("checked_index_result")
    )
}

pub(super) fn checked_range(start: &str, end: &str, inclusive: bool, len: &str) -> String {
    format!(
        "{}({start}, {end}, {inclusive}, {len})",
        rt_path("checked_range")
    )
}

#[cfg(test)]
mod tests {
    use super::{
        anv_list_ty, anv_map_from_entries, anv_map_ty, anv_string_from, box_pin_struct_start,
        callback_check_identity, callback_record_heap_type_field, checked_index, checked_range,
        ctx_roots_ty, dataref_place_heap_type_access, dataref_place_heap_type_field,
        dataref_place_ops_ty, erased_handle_ty, generated_call, generated_runtime_inner_symbol,
        generated_runtime_symbol, global_begin_projected_loan, global_set_or_replace_collection,
        heap_access_error, heap_register, heap_scope, heap_scope_owned, heap_type_access,
        lambda_cell_ctor, map_heap_access_error, mut_place_access, mut_place_dataref,
        mut_place_get_copy, mut_place_global, mut_place_heap_cell, mut_place_local,
        mut_place_local_raw, mut_place_projected, mut_place_reborrow, mut_place_replace_collection,
        mut_place_scoped_cell, mut_place_set, mut_place_stack_cell, mut_place_ty,
        non_null_cast_mut, optional_payload_ops_ctor, optional_payload_ops_ty, owner_attach,
        owner_begin_shutdown, owner_enter, owner_enter_current, owner_entry_ptr,
        owner_suspend_for_provider, phantom_pinned_ty, phantom_pinned_value, pin_box_ty,
        pin_get_unchecked_mut, projection_ops_ty, result_ty, rt_heap_alloc, rt_heap_erase,
        rt_heap_try_with_erased, rt_heap_try_with_erased_mut, rt_heap_with, rt_heap_with_mut,
        runtime_ctx_ty_with, runtime_owner_handle_new, runtime_owner_handle_ty, runtime_param_name,
        scoped_mut_place_cell_new, scoped_mut_place_cell_ty, stack_lambda_cell_ctor,
        stack_lambda_cell_ty, trace_crate_attr, trace_derive, visitor_ty,
    };

    #[test]
    fn renders_runtime_types() {
        assert_eq!(anv_list_ty("i64"), "anvyx_runtime::AnvList<'cx, i64>");
        assert_eq!(
            anv_map_ty("i64", "bool"),
            "anvyx_runtime::AnvMap<'cx, i64, bool>"
        );
        assert_eq!(
            stack_lambda_cell_ty("i64"),
            "anvyx_runtime::StackLambdaCell<i64>"
        );
        assert_eq!(mut_place_ty(), "anvyx_runtime::MutPlace");
        assert_eq!(
            dataref_place_ops_ty("i64"),
            "anvyx_runtime::DataRefPlaceOps<'cx, i64>"
        );
        assert_eq!(
            projection_ops_ty("Point", "i64"),
            "anvyx_runtime::ProjectionOps<'cx, Point, i64>"
        );
        assert_eq!(
            optional_payload_ops_ty("i64"),
            "anvyx_runtime::OptionalPayloadOps<i64>"
        );
        assert_eq!(erased_handle_ty(), "anvyx_runtime::ErasedHandle<'cx>");
        assert_eq!(
            scoped_mut_place_cell_ty("'env", "i64"),
            "anvyx_runtime::ScopedMutPlaceCell<'env, 'cx, i64>"
        );
        assert_eq!(runtime_ctx_ty_with("'_"), "anvyx_runtime::Ctx<'cx, '_>");
        assert_eq!(generated_runtime_symbol(), "AnvRuntime");
        assert_eq!(generated_runtime_inner_symbol(), "AnvRuntimeInner");
        assert_eq!(
            pin_box_ty("AnvRuntimeInner<'cx>"),
            "std::pin::Pin<Box<AnvRuntimeInner<'cx>>>"
        );
        assert_eq!(box_pin_struct_start("Inner"), "Box::pin(Inner {");
        assert_eq!(phantom_pinned_ty(), "std::marker::PhantomPinned");
        assert_eq!(phantom_pinned_value(), "std::marker::PhantomPinned");
        assert_eq!(
            runtime_owner_handle_ty(),
            "anvyx_runtime::RuntimeOwnerHandle"
        );
        assert_eq!(
            runtime_owner_handle_new(),
            "anvyx_runtime::RuntimeOwnerHandle::new()"
        );
        assert_eq!(
            owner_attach("owner", "ptr"),
            "owner.__anvyx_attach_owner_ptr(ptr)"
        );
        assert_eq!(
            owner_begin_shutdown("owner"),
            "owner.__anvyx_begin_shutdown()"
        );
        assert_eq!(
            callback_check_identity("key", 2, 3),
            "key.__anvyx_check_identity(std::num::NonZeroU64::new(2).unwrap(), std::num::NonZeroU64::new(3).unwrap())"
        );
        assert_eq!(callback_record_heap_type_field(0), "callback_record_sig0");
        assert_eq!(
            owner_enter_current("owner"),
            "owner.__anvyx_enter_current()"
        );
        assert_eq!(
            owner_enter("owner", "owner_id", "generation"),
            "owner.__anvyx_enter(owner_id, generation)"
        );
        assert_eq!(
            owner_suspend_for_provider("owner"),
            "owner.__anvyx_suspend_entry_for_provider()"
        );
        assert_eq!(owner_entry_ptr("entry"), "entry.owner_ptr()");
        assert_eq!(
            non_null_cast_mut("ptr", "Inner<'cx>"),
            "ptr.cast::<Inner<'cx>>().as_mut()"
        );
        assert_eq!(
            pin_get_unchecked_mut("runtime.inner.as_mut()"),
            "runtime.inner.as_mut().get_unchecked_mut()"
        );
        assert_eq!(
            stack_lambda_cell_ctor("i64"),
            "anvyx_runtime::StackLambdaCell::<i64>"
        );
        assert_eq!(lambda_cell_ctor("i64"), "anvyx_runtime::LambdaCell::<i64>");
        assert_eq!(
            scoped_mut_place_cell_new("v0"),
            "anvyx_runtime::ScopedMutPlaceCell::new(v0)"
        );
        assert_eq!(
            optional_payload_ops_ctor("i64"),
            "anvyx_runtime::OptionalPayloadOps::<i64>::default()"
        );
        assert_eq!(result_ty("i64"), "Result<i64, anvyx_runtime::RuntimeError>");
        assert_eq!(result_ty("()"), "Result<(), anvyx_runtime::RuntimeError>");
    }

    #[test]
    fn renders_trace_attrs() {
        assert_eq!(
            trace_derive(&["Clone", "Copy"]),
            "#[derive(Clone, Copy, anvyx_runtime::Trace)]"
        );
        assert_eq!(trace_crate_attr(false), "#[trace(crate = anvyx_runtime)]");
        assert_eq!(
            trace_crate_attr(true),
            "#[trace(crate = anvyx_runtime, ctx = 'cx)]"
        );
    }

    #[test]
    fn renders_runtime_calls() {
        assert_eq!(
            anv_string_from("out"),
            "anvyx_runtime::AnvString::from(out)"
        );
        assert_eq!(
            anv_map_from_entries("rt", "types.map_storage1", "(k, v)"),
            "anvyx_runtime::AnvMap::from_entries(rt, types.map_storage1, [(k, v)])"
        );
        assert_eq!(ctx_roots_ty(), "anvyx_runtime::CtxRoots");
        assert_eq!(visitor_ty("D"), "anvyx_runtime::Visitor<'cx, '_, D>");
        assert_eq!(
            generated_call("f", ["x".to_string()]),
            "f(rt, types, globals, x)"
        );
        assert_eq!(
            global_set_or_replace_collection("globals.xs", "next"),
            "globals.xs.set_without_init_or_replace(next, |slot, value| slot.replace_with(value))?"
        );
        assert_eq!(
            checked_index("i", "xs.len()"),
            "anvyx_runtime::checked_index(i, xs.len())"
        );
        assert_eq!(
            checked_range("start", "end", true, "xs.len()"),
            "anvyx_runtime::checked_range(start, end, true, xs.len())"
        );
    }

    #[test]
    fn renders_heap_api_spelling() {
        assert_eq!(heap_register(true), "register_tracked");
        assert_eq!(heap_register(false), "register_untracked");
        assert_eq!(
            rt_heap_alloc("rt", "heap_type", "Storage { value: 1 }"),
            "rt.heap().alloc(heap_type, Storage { value: 1 })"
        );
        assert_eq!(
            rt_heap_with("rt", "&v0", "item", "item.value"),
            "rt.heap().with(&v0, |item| item.value)"
        );
        assert_eq!(
            rt_heap_with_mut("rt", "&v0", "item", "item.value = 1;"),
            "rt.heap().with_mut(&v0, |item| { item.value = 1; })"
        );
        assert_eq!(heap_access_error(), "anvyx_runtime::heap_access_error");
        assert_eq!(
            map_heap_access_error("rt.heap().erase(&v0)"),
            "rt.heap().erase(&v0).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(
            rt_heap_erase("rt", "&v0"),
            "rt.heap().erase(&v0).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(
            rt_heap_try_with_erased(
                "rt",
                "object",
                "self.heap_type",
                "storage",
                "Node",
                "f(&storage.value)"
            ),
            "rt.heap().try_with_erased(object, self.heap_type, |storage: &Node| f(&storage.value)).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(
            rt_heap_try_with_erased_mut(
                "rt",
                "object",
                "self.heap_type",
                "storage",
                "Node",
                "f(&mut storage.value)"
            ),
            "rt.heap().try_with_erased_mut(object, self.heap_type, |storage: &mut Node| f(&mut storage.value)).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(runtime_param_name(), "rt");
        assert_eq!(heap_type_access("types", "node"), "types.node");
        assert_eq!(
            mut_place_local("slot"),
            "anvyx_runtime::MutPlace::local(&mut slot)"
        );
        assert_eq!(
            mut_place_local_raw("slot"),
            "unsafe { anvyx_runtime::MutPlace::local_raw(&raw mut slot) }"
        );
        assert_eq!(mut_place_reborrow("place"), "place.reborrow()");
        assert_eq!(
            mut_place_stack_cell("cell"),
            "anvyx_runtime::MutPlace::stack_cell(&cell)"
        );
        assert_eq!(
            mut_place_heap_cell("cell"),
            "anvyx_runtime::MutPlace::heap_cell(cell.clone())"
        );
        assert_eq!(
            mut_place_global("globals.g", "ginit(rt, types, globals)"),
            "anvyx_runtime::MutPlace::global(&globals.g, &|rt| ginit(rt, types, globals))"
        );
        assert_eq!(
            mut_place_scoped_cell("&cell"),
            "anvyx_runtime::MutPlace::scoped_cell(&cell)"
        );
        assert_eq!(
            mut_place_dataref("object", "&ops"),
            "anvyx_runtime::MutPlace::dataref(object, &ops)"
        );
        assert_eq!(
            mut_place_projected("root", "&ops"),
            "anvyx_runtime::MutPlace::projected(root, &ops)"
        );
        assert_eq!(dataref_place_heap_type_field(), "heap_type");
        assert_eq!(dataref_place_heap_type_access("self"), "self.heap_type");
        assert_eq!(
            mut_place_set("place", "rt", "value"),
            "place.set(rt, value)?"
        );
        assert_eq!(
            mut_place_access("place", "rt", "Ok(value.share())"),
            "place.access(rt, |value| Ok(value.share()))?"
        );
        assert_eq!(
            global_begin_projected_loan("globals.state"),
            "globals.state.begin_projected_loan()"
        );
        assert_eq!(
            mut_place_replace_collection("place", "rt", "next"),
            "place.mutate(rt, |slot| slot.replace_with(next))?"
        );
        assert_eq!(mut_place_get_copy("place", "rt"), "place.get_copy(rt)?");
        assert_eq!(heap_scope(), "anvyx_runtime::Heap::scope");
        assert_eq!(heap_scope_owned(), "anvyx_runtime::Heap::scope_owned");
    }
}
