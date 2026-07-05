use anvyx_runtime::{AnvyxRef, EscapingLambda};

#[derive(AnvyxRef)]
struct CallbackHolder {
    callback: Option<EscapingLambda<(), ()>>,
}

fn main() {}
