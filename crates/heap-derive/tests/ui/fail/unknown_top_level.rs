#[derive(anvyx_heap::Trace)]
#[trace(nope)]
struct Bad {
    value: i32,
}

fn main() {}
