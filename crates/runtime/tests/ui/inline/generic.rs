use anvyx_runtime::AnvyxInline;

#[derive(Clone, Copy, AnvyxInline)]
struct Boxed<T> {
    #[anvyx(field)]
    value: T,
}

fn main() {}
