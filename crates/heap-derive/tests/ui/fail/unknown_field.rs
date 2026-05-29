#[derive(anvyx_heap::Trace)]
struct Bad {
    #[trace(nope)]
    value: i32,
}

fn main() {}
