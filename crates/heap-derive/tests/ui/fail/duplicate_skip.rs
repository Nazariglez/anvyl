#[derive(anvyx_heap::Trace)]
struct Bad {
    #[trace(skip_unchecked, skip_unchecked)]
    value: i32,
}

fn main() {}
