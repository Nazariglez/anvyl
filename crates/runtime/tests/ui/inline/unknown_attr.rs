use anvyx_runtime::AnvyxInline;

#[derive(Clone, Copy, AnvyxInline)]
#[anvyx(shared)]
struct Point {
    #[anvyx(field)]
    pub x: i64,
}

fn main() {}
