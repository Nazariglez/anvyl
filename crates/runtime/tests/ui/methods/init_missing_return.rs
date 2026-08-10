use anvyx_runtime::methods;

struct MissingReturn;

#[methods]
impl MissingReturn {
    #[anvyx(init)]
    pub fn new() {}
}

fn main() {}
