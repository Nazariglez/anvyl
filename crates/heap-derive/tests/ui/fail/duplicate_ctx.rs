#[derive(anvyx_heap::Trace)]
#[trace(ctx = 'cx, ctx = 'cx)]
struct Bad<'cx> {
    value: std::marker::PhantomData<&'cx ()>,
}

fn main() {}
