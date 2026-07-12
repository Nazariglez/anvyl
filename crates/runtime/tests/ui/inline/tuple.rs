use anvyx_runtime::AnvyxInline;

#[derive(Clone, Copy, AnvyxInline)]
struct Pair(#[anvyx(field)] i64, #[anvyx(field)] i64);

fn main() {}
