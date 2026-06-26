use anvyx_runtime::methods;

struct Point { x: i64 }

#[methods]
impl Point {
    #[anvyx(init, ret = "int")]
    pub fn new(x: i64) -> Self { Self { x } }
}

fn main() {}
