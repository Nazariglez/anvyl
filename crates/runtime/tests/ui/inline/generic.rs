use anvyx_runtime::AnvyxInline;

#[derive(AnvyxInline)]
struct Boxed<T> {
    #[anvyx(field)]
    value: T,
}

fn main() {}
