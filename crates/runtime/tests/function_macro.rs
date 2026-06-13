#![allow(dead_code)]

use anvyx_runtime::{
    CallbackEscape, CallbackThread, Ctx, ExternCallbackParam, ExternCallbackSignature,
    ExternTypeExpr, Heap, MutPlace, ParamFlow, RuntimeError, RustAbiSupport, RustParamAbi,
    RustWrapperCtx, ScopedLambda, function,
};

/// Adds numbers.
#[function]
fn add(a: i64, b: i64) -> i64 {
    a + b
}

#[function(name = "line")]
fn draw_line(message: &str) {
    let _ = message;
}

#[function(ret = "int", params(value = "int"))]
fn renamed_type(value: i64) -> i64 {
    value
}

#[function]
fn maybe(value: Option<i64>) -> Result<Option<i64>, RuntimeError> {
    if value == Some(i64::MIN) {
        Err(RuntimeError::new("sentinel"))
    } else {
        Ok(value)
    }
}

#[function]
fn maybe_return(value: i64) -> Option<i64> {
    (value >= 0).then_some(value)
}

#[function]
fn strings(values: Vec<String>) -> Vec<String> {
    values
}

#[function(ctx)]
fn with_ctx(ctx: &mut Ctx<'_, '_>, value: i64) -> i64 {
    let _ = ctx.heap();
    value + 1
}

#[function(ctx)]
fn with_ctx_lifetime<'cx>(ctx: &mut Ctx<'cx, '_>, name: &'cx str) -> i64 {
    let _ = ctx.heap();
    name.len() as i64
}

#[function(ctx)]
fn bump_place<'cx>(ctx: &mut Ctx<'cx, '_>, value: MutPlace<'_, 'cx, i64>) {
    let _ = ctx.heap();
    let _ = value;
}

#[function(ctx)]
fn maybe_place<'cx>(ctx: &mut Ctx<'cx, '_>, value: MutPlace<'_, 'cx, Option<i64>>) {
    let _ = ctx.heap();
    let _ = value;
}

#[function]
fn each(f: ScopedLambda<'_, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    f.call(1)
}

#[function(params(f = "fn(int) -> bool"))]
fn callback_override(f: ScopedLambda<'_, '_, (i64,), bool>) -> Result<(), RuntimeError> {
    let _ = f;
    Ok(())
}

#[test]
fn descriptor_contains_params_return_effects_and_docs() {
    let export = __anvyx_export_add();

    assert_eq!(export.descriptor.name, "add");
    assert_eq!(export.descriptor.doc.as_deref(), Some("Adds numbers."));
    assert_eq!(export.descriptor.signature.params.len(), 2);
    assert_eq!(
        export.descriptor.signature.params[0].name.as_deref(),
        Some("a")
    );
    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::Int
    );
    assert_eq!(export.descriptor.signature.params[0].flow, ParamFlow::Value);
    assert_eq!(export.descriptor.signature.ret, ExternTypeExpr::Int);
    assert!(!export.descriptor.effects.fallible);
    assert_eq!(export.rust.symbol, "add");
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);
}

#[test]
fn name_override_and_borrowed_string_flow() {
    let export = __anvyx_export_draw_line();

    assert_eq!(export.descriptor.name, "line");
    assert_eq!(export.rust.symbol, "line");
    assert_eq!(export.descriptor.signature.ret, ExternTypeExpr::Void);
    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::String
    );
    assert_eq!(
        export.descriptor.signature.params[0].flow,
        ParamFlow::Borrow
    );
}

#[test]
fn type_overrides_are_parsed_into_descriptors() {
    let export = __anvyx_export_renamed_type();

    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::Int
    );
    assert_eq!(export.descriptor.signature.ret, ExternTypeExpr::Int);
}

#[test]
fn option_result_and_list_support_metadata() {
    let export = __anvyx_export_maybe();

    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::Option(Box::new(ExternTypeExpr::Int))
    );
    assert_eq!(
        export.descriptor.signature.ret,
        ExternTypeExpr::Option(Box::new(ExternTypeExpr::Int))
    );
    assert!(export.descriptor.effects.fallible);
    assert!(export.rust.abi.fallible);
    assert_eq!(export.rust.abi.support, RustAbiSupport::Unsupported);

    let export = __anvyx_export_maybe_return();
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);

    let export = __anvyx_export_strings();
    assert_eq!(export.rust.abi.support, RustAbiSupport::Unsupported);
}

#[test]
fn ctx_is_hidden_from_metadata_and_passed_to_authored_function() {
    let export = __anvyx_export_with_ctx();

    assert_eq!(export.descriptor.signature.params.len(), 1);
    assert_eq!(export.rust.abi.params.len(), 1);
    assert_eq!(
        export.rust.abi.params[0],
        RustParamAbi::Value(ExternTypeExpr::Int)
    );
    Heap::scope(|heap| {
        let mut ctx = Ctx::new(heap);
        assert_eq!(__anvyx_native_export_with_ctx::with_ctx(&mut ctx, 41), 42);
    });

    let export = __anvyx_export_with_ctx_lifetime();
    assert_eq!(export.descriptor.signature.params.len(), 1);
}

#[test]
fn scoped_lambda_param_infers_callback_descriptor_and_abi() {
    let export = __anvyx_export_each();
    let expected = ExternCallbackSignature {
        params: vec![ExternCallbackParam {
            ty: ExternTypeExpr::Int,
            escape: CallbackEscape::NonEscaping,
        }],
        ret: Box::new(ExternTypeExpr::Void),
        policy: anvyx_runtime::CallbackPolicy {
            escape: CallbackEscape::NonEscaping,
            thread: CallbackThread::SameThread,
        },
    };

    assert_eq!(export.descriptor.signature.params.len(), 1);
    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::Callback(expected.clone())
    );
    assert_eq!(
        export.rust.abi.params[0],
        RustParamAbi::ScopedLambda(expected)
    );
    assert_eq!(
        export.rust.abi.support,
        RustAbiSupport::NeedsWrapperConversion
    );
    assert_eq!(export.rust.abi.ctx, RustWrapperCtx::None);
}

#[test]
fn scoped_lambda_override_must_match_rust_abi() {
    let export = __anvyx_export_callback_override();

    let ExternTypeExpr::Callback(callback) = &export.descriptor.signature.params[0].ty else {
        panic!("expected callback descriptor");
    };
    assert_eq!(*callback.ret, ExternTypeExpr::Bool);
}

#[test]
fn mut_place_param_uses_place_aware_mutable_abi() {
    let export = __anvyx_export_bump_place();

    assert_eq!(export.descriptor.signature.params.len(), 1);
    assert_eq!(
        export.descriptor.signature.params[0].flow,
        ParamFlow::MutBorrow
    );
    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::Int
    );
    assert_eq!(export.rust.abi.params.len(), 1);
    assert_eq!(
        export.rust.abi.params[0],
        RustParamAbi::MutPlace(ExternTypeExpr::Int)
    );
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);

    let export = __anvyx_export_maybe_place();
    assert_eq!(
        export.rust.abi.params[0],
        RustParamAbi::MutPlace(ExternTypeExpr::Option(Box::new(ExternTypeExpr::Int)))
    );
}
