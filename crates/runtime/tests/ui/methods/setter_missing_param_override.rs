use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(setter, params(missing = "int"))]
    pub fn x(&mut self, value: i64) {}
}

fn main() {}
