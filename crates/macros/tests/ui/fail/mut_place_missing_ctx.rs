use anvyx_runtime::{function, MutPlace};

#[function]
fn bad(value: MutPlace<'_, '_, i64>) {
    drop(value);
}

fn main() {}
