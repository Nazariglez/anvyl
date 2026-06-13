use anvyx_runtime::ScopedLambda;

fn assert_clone<T: Clone>() {}

fn main() {
    assert_clone::<ScopedLambda<'static, 'static, (), ()>>();
}
