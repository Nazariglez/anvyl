use anvyx_runtime::methods;

struct Host;

#[methods]
impl Host {
    #[anvyx(init)]
    pub fn new(f: anvyx_runtime::EscapingLambda<(), ()>) -> Self {
        drop(f);
        Self
    }
}

fn main() {}
