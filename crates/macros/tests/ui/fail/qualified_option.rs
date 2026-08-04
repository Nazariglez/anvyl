use anvyx_runtime::function;

mod foreign {
    pub struct Option<T>(pub T);
}

#[function]
fn bad(value: foreign::Option<i64>) {
    drop(value);
}

fn main() {}
