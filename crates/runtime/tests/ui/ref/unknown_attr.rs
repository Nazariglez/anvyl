use anvyx_runtime::AnvyxRef;

#[derive(AnvyxRef)]
#[anvyx(inline)]
struct Counter {
    #[anvyx(field)]
    count: i64,
}

fn main() {}
