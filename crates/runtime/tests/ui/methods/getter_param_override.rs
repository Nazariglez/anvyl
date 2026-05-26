use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(getter, params(value = "int"))]
    pub fn x(&self) -> i64 { 1 }
}

fn main() {}
