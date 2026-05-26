use anvyx_runtime::function;

#[function]
fn bad(value: f32) -> f64 { value as f64 }

fn main() {}
