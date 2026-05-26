use anvyx_runtime::AnvyxRef;

#[derive(AnvyxRef)]
struct Boxed<T> {
    #[anvyx(field)]
    value: T,
}

fn main() {}
