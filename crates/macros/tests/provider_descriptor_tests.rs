use anvyx_externs::{CallbackEscape, CallbackThread, ExternTypeExpr};
use anvyx_macros::provider_descriptor;

#[test]
fn parses_callback_param() {
    let descriptor = provider_descriptor! {
        provider = "host",
        module = "host",
        fn each(f: fn(int) -> bool) -> void;
    };
    let param = &descriptor.modules[0].functions[0].signature.params[0];
    let ExternTypeExpr::Callback(callback) = &param.ty else {
        panic!("expected callback type");
    };

    assert_eq!(callback.params[0].ty, ExternTypeExpr::Int);
    assert_eq!(callback.params[0].escape, CallbackEscape::NonEscaping);
    assert_eq!(*callback.ret, ExternTypeExpr::Bool);
    assert_eq!(callback.policy.escape, CallbackEscape::NonEscaping);
    assert_eq!(callback.policy.thread, CallbackThread::SameThread);
}

#[test]
fn parses_void_callback_shorthand() {
    let descriptor = provider_descriptor! {
        provider = "host",
        module = "host",
        fn each(f: fn(float)) -> void;
    };
    let ExternTypeExpr::Callback(callback) =
        &descriptor.modules[0].functions[0].signature.params[0].ty
    else {
        panic!("expected callback type");
    };

    assert_eq!(*callback.ret, ExternTypeExpr::Void);
}
