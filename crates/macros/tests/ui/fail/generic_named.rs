use anvyx_runtime::function;

struct WindowConfig<T>(T);

#[function]
fn bad(value: WindowConfig<i64>) {
    drop(value);
}

fn main() {}
