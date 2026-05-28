use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(getter)]
    pub fn x(&self) -> i64 { 0 }
}

fn main() {}
