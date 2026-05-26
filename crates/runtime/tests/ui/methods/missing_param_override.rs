use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(params(missing = "int"))]
    pub fn x(&self, value: i64) -> i64 { value }
}

fn main() {}
