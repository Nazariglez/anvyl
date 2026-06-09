use anvyx_runtime::{ScopedMutPlaceCell, StackLambdaCell};

fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}

fn main() {
    assert_send::<StackLambdaCell<i32>>();
    assert_sync::<StackLambdaCell<i32>>();
    assert_send::<ScopedMutPlaceCell<'static, 'static, i32>>();
    assert_sync::<ScopedMutPlaceCell<'static, 'static, i32>>();
}
