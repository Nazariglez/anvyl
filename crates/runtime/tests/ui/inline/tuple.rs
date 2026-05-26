use anvyx_runtime::AnvyxInline;

#[derive(AnvyxInline)]
struct Pair(#[anvyx(field)] i64, #[anvyx(field)] i64);

fn main() {}
