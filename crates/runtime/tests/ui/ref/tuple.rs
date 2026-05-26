use anvyx_runtime::AnvyxRef;

#[derive(AnvyxRef)]
struct Pair(#[anvyx(field)] i64, #[anvyx(field)] i64);

fn main() {}
