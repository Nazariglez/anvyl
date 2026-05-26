use anvyx_runtime::AnvyxInline;

#[derive(AnvyxInline)]
#[anvyx(shared)]
struct Point {
    #[anvyx(field)]
    x: i64,
}

fn main() {}
