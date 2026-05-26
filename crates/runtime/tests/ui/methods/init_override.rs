use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(init, ret = "int")]
    pub fn new() -> Self { Self }
}

fn main() {}
