use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    pub fn consume(self) {}
}

fn main() {}
