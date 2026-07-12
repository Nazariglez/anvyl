use anvyx_runtime::AnvyxInline;

#[derive(Clone, Copy, AnvyxInline)]
struct Point {
    #[anvyx(getter)]
    x: i64,
}

fn main() {}
