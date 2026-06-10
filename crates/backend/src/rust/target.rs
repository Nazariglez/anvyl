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

pub(super) fn stack_lambda_cell_ctor(payload: &str) -> String {
    format!("{}::<{payload}>", rt_path("StackLambdaCell"))
}

pub(super) fn lambda_cell_ctor(payload: &str) -> String {
    format!("{}::<{payload}>", rt_path("LambdaCell"))
}

pub(super) fn runtime_error_ty() -> String {
    rt_path("RuntimeError")
}

pub(super) fn result_ty(ret: &str) -> String {
    format!("Result<{ret}, {}>", runtime_error_ty())
}

pub(super) fn runtime_ctx_ty() -> String {
    format!("{}<'cx, 'rt>", rt_path("Ctx"))
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

pub(super) fn anv_list_default() -> String {
    format!("{}::default()", rt_path("AnvList"))
}

pub(super) fn anv_map_from_entries(entries: &str) -> String {
    format!("{}::from_entries([{entries}])", rt_path("AnvMap"))
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

pub(super) fn ctx_runtime(ctx: &str) -> String {
    format!("{ctx}.runtime()")
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
        checked_range, ctx_heap_alloc, ctx_heap_with, ctx_heap_with_mut, ctx_runtime,
        heap_register, heap_scope, lambda_cell_ctor, mut_place_ty, result_ty, runtime_ctx_new,
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
            stack_lambda_cell_ctor("i64"),
            "anvyx_runtime::StackLambdaCell::<i64>"
        );
        assert_eq!(lambda_cell_ctor("i64"), "anvyx_runtime::LambdaCell::<i64>");
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
        assert_eq!(ctx_runtime("ctx"), "ctx.runtime()");
        assert_eq!(heap_scope(), "anvyx_runtime::Heap::scope");
        assert_eq!(runtime_ctx_new("heap"), "anvyx_runtime::Ctx::new(heap)");
    }
}
