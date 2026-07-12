use anvyx_runtime::{AnvyxInline, function};

#[derive(Clone, Copy, AnvyxInline)]
pub struct Bad {
    #[anvyx(field)]
    pub value: String,
}

#[function]
fn use_bad(value: Bad) { drop(value); }

fn main() {}
