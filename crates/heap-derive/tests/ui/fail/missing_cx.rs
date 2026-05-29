#[derive(anvyx_heap::Trace)]
struct Bad<'a> {
    value: &'a i32,
}

fn main() {}
