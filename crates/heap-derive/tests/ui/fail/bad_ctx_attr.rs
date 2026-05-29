#[derive(anvyx_heap::Trace)]
#[trace(ctx = 'ctx)]
struct Bad<'ctx> {
    value: std::marker::PhantomData<&'ctx ()>,
}

fn main() {}
