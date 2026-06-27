use anvyx_runtime::{AnvyxInline, function};

#[derive(Clone, AnvyxInline)]
struct Bad {
    #[anvyx(field)]
    value: String,
}

#[function]
fn use_bad(value: Bad) { drop(value); }

fn main() {}
