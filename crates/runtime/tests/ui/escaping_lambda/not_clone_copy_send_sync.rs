use anvyx_runtime::EscapingLambda;

fn assert_clone<T: Clone>() {}
fn assert_copy<T: Copy>() {}
fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}

fn main() {
    assert_clone::<EscapingLambda<(), ()>>();
    assert_copy::<EscapingLambda<(), ()>>();
    assert_send::<EscapingLambda<(), ()>>();
    assert_sync::<EscapingLambda<(), ()>>();
}
