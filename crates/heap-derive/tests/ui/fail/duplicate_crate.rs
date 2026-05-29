#[derive(anvyx_heap::Trace)]
#[trace(crate = anvyx_heap, crate = ::anvyx_heap)]
struct Bad {
    value: i32,
}

fn main() {}
