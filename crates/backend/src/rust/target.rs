use super::syntax;

const RT: &str = "anvyx_runtime";

fn rt_path(item: &str) -> String {
    format!("{RT}::{item}")
}

pub(super) fn anv_string_ty() -> String {
    rt_path("AnvString")
}

pub(super) fn anv_list_ty(elem: String) -> String {
    format!("{}<{elem}>", rt_path("AnvList"))
}

pub(super) fn anv_slice_ty(elem: String) -> String {
    format!("{}<{elem}>", rt_path("AnvSlice"))
}

pub(super) fn anv_map_ty(key: String, value: String) -> String {
    format!("{}<{key}, {value}>", rt_path("AnvMap"))
}

pub(super) fn stack_lambda_cell_ty(payload: &str) -> String {
    format!("{}<{payload}>", rt_path("StackLambdaCell"))
}

pub(super) fn lambda_cell_ty(payload: &str) -> String {
    format!("{}<{payload}>", rt_path("LambdaCell"))
}

pub(super) fn mut_place_ty() -> String {
    rt_path("MutPlace")
}

pub(super) fn dataref_place_ops_ty(payload: &str) -> String {
    format!("{}<'cx, {payload}>", rt_path("DataRefPlaceOps"))
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

pub(super) fn runtime_ctx_ty() -> String {
    runtime_ctx_ty_with("'rt")
}

pub(super) fn runtime_ctx_ty_with(rt_lifetime: &str) -> String {
    format!("{}<'cx, {rt_lifetime}>", rt_path("Ctx"))
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

pub(super) fn anv_string_from(expr: &str) -> String {
    format!("{}::from({expr})", anv_string_ty())
}

pub(super) fn anv_string_format(fmt: &str, arg: &str) -> String {
    anv_string_from(&format!("format!({fmt}, {arg})"))
}

pub(super) fn anv_list_from_elems(elems: &str) -> String {
    format!("{}::from_elems([{elems}])", rt_path("AnvList"))
}

pub(super) fn anv_list_from_iter(iter: &str) -> String {
    format!("{}::from_elems({iter})", rt_path("AnvList"))
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

pub(super) fn anv_slice_from_list(root: &str, start: &str, len: &str, guard: &str) -> String {
    format!(
        "unsafe {{ {}::from_list({root}, {start}, {len}, {guard}) }}",
        rt_path("AnvSlice")
    )
}

pub(super) fn anv_slice_from_list_mut(root: &str, start: &str, len: &str, guard: &str) -> String {
    format!(
        "unsafe {{ {}::from_list_mut({root}, {start}, {len}, {guard}) }}",
        rt_path("AnvSlice")
    )
}

pub(super) fn anv_slice_slice(source: &str, start: &str, len: &str) -> String {
    format!("{source}.slice({start}, {len})")
}

pub(super) fn anv_slice_copy_range_with(
    source: &str,
    range: &str,
    item: &str,
    body: &str,
) -> String {
    format!("unsafe {{ {source}.copy_range_with({range}, |{item}| {body}) }}")
}

pub(super) fn mut_place_slice_view(
    place: &str,
    start: &str,
    end: &str,
    inclusive: bool,
    mutable: bool,
) -> String {
    let method = if mutable {
        "slice_view_mut"
    } else {
        "slice_view"
    };
    format!("{place}.{method}({start}, {end}, {inclusive})?")
}

pub(super) fn anv_map_from_entries(entries: &str) -> String {
    format!("{}::from_entries([{entries}])", rt_path("AnvMap"))
}

pub(super) fn list_push(list: &str, value: &str) -> String {
    format!("{list}.push({value})?")
}

pub(super) fn list_push_region(value: &str) -> String {
    format!("{{ value.push({value})?; Ok(()) }}")
}

pub(super) fn map_insert(map: &str, key: &str, value: &str) -> String {
    format!("{map}.insert({key}, {value})?")
}

pub(super) fn map_insert_region(key: &str, inserted: &str) -> String {
    format!("{{ value.insert({key}, {inserted})?; Ok(()) }}")
}

pub(super) fn map_remove(map: &str, key: &str) -> String {
    format!("{map}.remove(&{key})?")
}

pub(super) fn map_remove_region(key: &str) -> String {
    format!("value.remove(&{key})")
}

pub(super) fn collection_structural_version(collection: &str) -> String {
    format!("{collection}.structural_version()")
}

pub(super) fn list_elem_at_shared(list: &str, index: &str, version: &str) -> String {
    format!("{list}.elem_at_shared({index}, {version})")
}

pub(super) fn list_with_elem_mut_short(
    list: &str,
    index: &str,
    version: &str,
    body: &str,
) -> String {
    format!("{list}.with_elem_mut_short({index}, {version}, |value| {{ {body} }})")
}

pub(super) fn slice_elem_at_shared(slice: &str, index: &str) -> String {
    format!("{slice}.elem_at_shared({index})")
}

pub(super) fn slice_with_elem_mut_short(slice: &str, index: &str, body: &str) -> String {
    format!("{slice}.with_elem_mut_short({index}, |value| {{ {body} }})")
}

pub(super) fn map_key_at_shared(map: &str, index: &str, version: &str) -> String {
    format!("{map}.key_at_shared({index}, {version})?")
}

pub(super) fn map_value_at_shared(map: &str, index: &str, version: &str) -> String {
    format!("{map}.value_at_shared({index}, {version})?")
}

pub(super) fn map_with_value_mut_short(
    map: &str,
    index: &str,
    version: &str,
    body: &str,
) -> String {
    format!("{map}.with_value_mut_short({index}, {version}, |value| {{ {body} }})")
}

pub(super) fn begin_shape_loan(root: &str) -> String {
    format!("{root}.begin_shape_loan()?")
}

pub(super) fn begin_shape_loan_region() -> String {
    "value.begin_shape_loan()".into()
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

pub(super) fn ctx_heap_alloc(ctx: &str, heap_type: &str, storage: &str) -> String {
    format!("{ctx}.heap().alloc({heap_type}, {storage})")
}

pub(super) fn ctx_heap_with(ctx: &str, object: &str, storage: &str, body: &str) -> String {
    format!("{ctx}.heap().with({object}, |{storage}| {body})")
}

pub(super) fn ctx_heap_with_mut(ctx: &str, object: &str, storage: &str, body: &str) -> String {
    format!("{ctx}.heap().with_mut({object}, |{storage}| {{ {body} }})")
}

pub(super) fn heap_access_error() -> String {
    rt_path("heap_access_error")
}

pub(super) fn map_heap_access_error(expr: &str) -> String {
    format!("{expr}.map_err({})?", heap_access_error())
}

pub(super) fn ctx_heap_erase(ctx: &str, object: &str) -> String {
    map_heap_access_error(&format!("{ctx}.heap().erase({object})"))
}

pub(super) fn ctx_heap_try_with_erased(
    ctx: &str,
    object: &str,
    heap_type: &str,
    storage: &str,
    storage_ty: &str,
    body: &str,
) -> String {
    ctx_heap_try_with_erased_op(ctx, object, heap_type, storage, storage_ty, body, false)
}

pub(super) fn ctx_heap_try_with_erased_mut(
    ctx: &str,
    object: &str,
    heap_type: &str,
    storage: &str,
    storage_ty: &str,
    body: &str,
) -> String {
    ctx_heap_try_with_erased_op(ctx, object, heap_type, storage, storage_ty, body, true)
}

fn ctx_heap_try_with_erased_op(
    ctx: &str,
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
        "{ctx}.heap().{method}({object}, {heap_type}, |{storage}: {storage_ref}{storage_ty}| {body})"
    ))
}

pub(super) fn ctx_runtime(ctx: &str) -> String {
    format!("{ctx}.runtime()")
}

pub(super) fn mut_place_dataref(object: &str, ops: &str) -> String {
    format!("{}::dataref({object}, {ops})", mut_place_ty())
}

pub(super) fn mut_place_set(place: &str, runtime: &str, value: &str) -> String {
    format!("{place}.set({runtime}, {value})?")
}

pub(super) fn mut_place_replace_collection(place: &str, runtime: &str, value: &str) -> String {
    format!("{place}.mutate({runtime}, |slot| slot.replace_with({value}))?")
}

pub(super) fn replace_collection(place: &str, value: &str) -> String {
    format!("{place}.replace_with({value})?")
}

pub(super) fn mut_place_access(place: &str, runtime: &str, body: &str) -> String {
    mut_place_region(place, "access", runtime, body)
}

pub(super) fn mut_place_get_copy(place: &str, runtime: &str) -> String {
    format!("{place}.get_copy({runtime})?")
}

pub(super) fn mut_place_region(place: &str, op: &str, runtime: &str, body: &str) -> String {
    format!("{place}.{op}({runtime}, |value| {body})?")
}

pub(super) fn heap_scope() -> String {
    rt_path("Heap::scope")
}

pub(super) fn runtime_ctx_new(heap: &str) -> String {
    format!("{}::new({heap})", rt_path("Ctx"))
}

pub(super) fn checked_index(index: &str, len: &str) -> String {
    format!("{}({index}, {len})", rt_path("checked_index"))
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
        anv_list_ty, anv_map_from_entries, anv_map_ty, anv_string_from, checked_index,
        checked_range, ctx_heap_alloc, ctx_heap_erase, ctx_heap_try_with_erased,
        ctx_heap_try_with_erased_mut, ctx_heap_with, ctx_heap_with_mut, ctx_runtime,
        dataref_place_ops_ty, erased_handle_ty, heap_access_error, heap_register, heap_scope,
        lambda_cell_ctor, map_heap_access_error, mut_place_access, mut_place_dataref,
        mut_place_get_copy, mut_place_set, mut_place_ty, result_ty, runtime_ctx_new,
        runtime_ctx_ty_with, scoped_mut_place_cell_new, scoped_mut_place_cell_ty,
        stack_lambda_cell_ctor, stack_lambda_cell_ty, trace_crate_attr, trace_derive, visitor_ty,
    };

    #[test]
    fn renders_runtime_types() {
        assert_eq!(
            anv_list_ty("i64".to_string()),
            "anvyx_runtime::AnvList<i64>"
        );
        assert_eq!(
            anv_map_ty("i64".to_string(), "bool".to_string()),
            "anvyx_runtime::AnvMap<i64, bool>"
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
        assert_eq!(erased_handle_ty(), "anvyx_runtime::ErasedHandle<'cx>");
        assert_eq!(
            scoped_mut_place_cell_ty("'env", "i64"),
            "anvyx_runtime::ScopedMutPlaceCell<'env, 'cx, i64>"
        );
        assert_eq!(runtime_ctx_ty_with("'_"), "anvyx_runtime::Ctx<'cx, '_>");
        assert_eq!(
            stack_lambda_cell_ctor("i64"),
            "anvyx_runtime::StackLambdaCell::<i64>"
        );
        assert_eq!(lambda_cell_ctor("i64"), "anvyx_runtime::LambdaCell::<i64>");
        assert_eq!(
            scoped_mut_place_cell_new("v0"),
            "anvyx_runtime::ScopedMutPlaceCell::new(v0)"
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
            anv_map_from_entries("(k, v)"),
            "anvyx_runtime::AnvMap::from_entries([(k, v)])"
        );
        assert_eq!(visitor_ty("D"), "anvyx_runtime::Visitor<'cx, '_, D>");
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
            ctx_heap_alloc("ctx", "heap_type", "Storage { value: 1 }"),
            "ctx.heap().alloc(heap_type, Storage { value: 1 })"
        );
        assert_eq!(
            ctx_heap_with("cx", "&v0", "item", "item.value"),
            "cx.heap().with(&v0, |item| item.value)"
        );
        assert_eq!(
            ctx_heap_with_mut("cx", "&v0", "item", "item.value = 1;"),
            "cx.heap().with_mut(&v0, |item| { item.value = 1; })"
        );
        assert_eq!(heap_access_error(), "anvyx_runtime::heap_access_error");
        assert_eq!(
            map_heap_access_error("ctx.heap().erase(&v0)"),
            "ctx.heap().erase(&v0).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(
            ctx_heap_erase("ctx", "&v0"),
            "ctx.heap().erase(&v0).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(
            ctx_heap_try_with_erased(
                "ctx",
                "object",
                "self.heap_type",
                "storage",
                "Node",
                "f(&storage.value)"
            ),
            "ctx.heap().try_with_erased(object, self.heap_type, |storage: &Node| f(&storage.value)).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(
            ctx_heap_try_with_erased_mut(
                "ctx",
                "object",
                "self.heap_type",
                "storage",
                "Node",
                "f(&mut storage.value)"
            ),
            "ctx.heap().try_with_erased_mut(object, self.heap_type, |storage: &mut Node| f(&mut storage.value)).map_err(anvyx_runtime::heap_access_error)?"
        );
        assert_eq!(ctx_runtime("ctx"), "ctx.runtime()");
        assert_eq!(
            mut_place_dataref("object", "&ops"),
            "anvyx_runtime::MutPlace::dataref(object, &ops)"
        );
        assert_eq!(
            mut_place_set("place", "ctx.runtime()", "value"),
            "place.set(ctx.runtime(), value)?"
        );
        assert_eq!(
            mut_place_access("place", "ctx.runtime()", "Ok(value.share())"),
            "place.access(ctx.runtime(), |value| Ok(value.share()))?"
        );
        assert_eq!(
            mut_place_get_copy("place", "ctx.runtime()"),
            "place.get_copy(ctx.runtime())?"
        );
        assert_eq!(heap_scope(), "anvyx_runtime::Heap::scope");
        assert_eq!(runtime_ctx_new("heap"), "anvyx_runtime::Ctx::new(heap)");
    }
}
