use anvyx_runtime::StackLambdaCell;

fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}

fn main() {
    assert_send::<StackLambdaCell<i32>>();
    assert_sync::<StackLambdaCell<i32>>();
}
