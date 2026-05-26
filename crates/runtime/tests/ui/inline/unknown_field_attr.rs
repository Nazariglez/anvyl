use anvyx_runtime::AnvyxInline;

#[derive(AnvyxInline)]
struct Point {
    #[anvyx(getter)]
    x: i64,
}

fn main() {}
