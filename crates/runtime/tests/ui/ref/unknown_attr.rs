use anvyx_runtime::AnvyxRef;

#[derive(AnvyxRef)]
#[anvyx(inline)]
struct Counter {
    #[anvyx(field)]
    pub count: i64,
}

fn main() {}
