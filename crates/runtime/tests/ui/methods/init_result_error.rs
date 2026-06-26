use anvyx_runtime::methods;

struct Fallible;

#[methods]
impl Fallible {
    #[anvyx(init)]
    pub fn new() -> Result<Self, anvyx_runtime::RuntimeError> { Ok(Self) }
}

fn main() {}
