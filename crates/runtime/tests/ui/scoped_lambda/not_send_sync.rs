use anvyx_runtime::ScopedLambda;

fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}

fn main() {
    assert_send::<ScopedLambda<'static, 'static, (), ()>>();
    assert_sync::<ScopedLambda<'static, 'static, (), ()>>();
}
