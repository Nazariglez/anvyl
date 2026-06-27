use anvyx_runtime::{AnvyxEnum, function};

#[derive(Clone, AnvyxEnum)]
enum Bad {
    Value(String),
}

#[function]
fn use_bad(value: Bad) { drop(value); }

fn main() {}
