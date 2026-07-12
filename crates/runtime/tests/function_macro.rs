#![allow(dead_code)]

use anvyx_runtime::{
    AnvList, AnvRef, AnvRefType, AnvString, AnvyxInline, AnvyxRef, CallbackEscape, CallbackThread,
    Ctx, EscapingLambda, ExternCallbackParam, ExternCallbackSignature, ExternTypeExpr, Heap,
    MutPlace, ParamFlow, RuntimeError, RuntimeResult, RustAbiSupport, RustParamAbi, RustWrapperCtx,
    ScopedLambda, function,
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

#[derive(Clone, Copy, AnvyxInline)]
#[anvyx(name = "Point")]
pub struct RustPoint {
    #[anvyx(field)]
    pub x: f64,
}

#[function(params(point = "Point"))]
fn point_x(point: RustPoint) -> f64 {
    point.x
}

#[function(ret = "Point")]
fn make_point() -> RustPoint {
    RustPoint { x: 1.0 }
}

#[function]
fn maybe(value: Option<i64>) -> RuntimeResult<Option<i64>> {
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

#[function(ctx)]
fn strings<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    values: AnvList<'cx, AnvString>,
) -> AnvList<'cx, AnvString> {
    let _ = ctx.heap();
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
    drop(value);
}

#[function(ctx)]
fn maybe_place<'cx>(ctx: &mut Ctx<'cx, '_>, value: MutPlace<'_, 'cx, Option<i64>>) {
    let _ = ctx.heap();
    drop(value);
}

#[function]
fn each(f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    let result = f.call(1);
    let _ = std::hint::black_box(f);
    result
}

#[function(params(f = "fn(int) -> bool"))]
fn callback_override(f: ScopedLambda<'_, '_, (i64,), bool>) -> RuntimeResult<()> {
    let result = f.call(1).map(|_| ());
    let _ = std::hint::black_box(f);
    result
}

#[function]
fn retain_callback(f: EscapingLambda<(i64,), ()>) {
    drop(f);
}

#[derive(Clone, Copy, AnvyxInline)]
pub struct LoadError {
    #[anvyx(field)]
    pub code: i64,
}

#[function]
fn visible_result(ok: bool) -> Result<i64, LoadError> {
    if ok {
        Ok(1)
    } else {
        Err(LoadError { code: 404 })
    }
}

#[function]
fn fallible_visible_result(ok: bool) -> RuntimeResult<Result<i64, LoadError>> {
    if ok {
        Ok(Ok(1))
    } else {
        Err(RuntimeError::new("hidden"))
    }
}

#[derive(AnvyxRef)]
pub struct Counter {
    #[anvyx(field)]
    pub value: i64,
}

#[function(ctx)]
fn make_counter<'cx>(ctx: &mut Ctx<'cx, '_>) -> AnvRef<'cx, Counter> {
    AnvRefType::<Counter>::register_untracked_in(ctx).alloc_in(ctx, Counter { value: 1 })
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
fn named_type_override_updates_descriptor_and_rust_abi() {
    let export = __anvyx_export_point_x();
    let point = ExternTypeExpr::Named {
        module: None,
        name: "Point".to_string(),
        args: vec![],
    };

    assert_eq!(export.descriptor.signature.params[0].ty, point.clone());
    assert_eq!(
        export.rust.abi.params[0],
        RustParamAbi::OwnedNamed(point.clone())
    );

    let export = __anvyx_export_make_point();
    assert_eq!(export.descriptor.signature.ret, point.clone());
    assert_eq!(
        export.rust.abi.ret,
        anvyx_runtime::RustReturnAbi::OwnedNamed(point)
    );
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
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);

    let export = __anvyx_export_maybe_return();
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);

    let export = __anvyx_export_strings();
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);

    let export = __anvyx_export_visible_result();
    let err = ExternTypeExpr::Named {
        module: None,
        name: "LoadError".to_string(),
        args: vec![],
    };
    assert_eq!(
        export.descriptor.signature.ret,
        ExternTypeExpr::Result(Box::new(ExternTypeExpr::Int), Box::new(err.clone()))
    );
    assert!(!export.descriptor.effects.fallible);
    assert_eq!(
        export.rust.abi.ret,
        anvyx_runtime::RustReturnAbi::Result(
            Box::new(anvyx_runtime::RustReturnAbi::Value(ExternTypeExpr::Int)),
            Box::new(anvyx_runtime::RustReturnAbi::OwnedNamed(err.clone())),
        )
    );
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);

    let export = __anvyx_export_fallible_visible_result();
    assert!(export.descriptor.effects.fallible);
    assert_eq!(
        export.descriptor.signature.ret,
        ExternTypeExpr::Result(Box::new(ExternTypeExpr::Int), Box::new(err))
    );

    let export = __anvyx_export_make_counter();
    let counter = ExternTypeExpr::Named {
        module: None,
        name: "Counter".to_string(),
        args: vec![],
    };
    assert_eq!(export.descriptor.signature.ret, counter.clone());
    assert_eq!(
        export.rust.abi.ret,
        anvyx_runtime::RustReturnAbi::Value(counter)
    );
    assert_eq!(export.rust.abi.support, RustAbiSupport::Direct);
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
fn escaping_lambda_param_infers_escaping_callback_descriptor_and_abi() {
    let export = __anvyx_export_retain_callback();
    let expected = ExternCallbackSignature {
        params: vec![ExternCallbackParam {
            ty: ExternTypeExpr::Int,
            escape: CallbackEscape::NonEscaping,
        }],
        ret: Box::new(ExternTypeExpr::Void),
        policy: anvyx_runtime::CallbackPolicy {
            escape: CallbackEscape::Escaping,
            thread: CallbackThread::SameThread,
        },
    };

    assert_eq!(
        export.descriptor.signature.params[0].escape,
        CallbackEscape::Escaping
    );
    assert_eq!(
        export.descriptor.signature.params[0].ty,
        ExternTypeExpr::Callback(expected.clone())
    );
    assert_eq!(
        export.rust.abi.params[0],
        RustParamAbi::EscapingLambda(expected)
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
