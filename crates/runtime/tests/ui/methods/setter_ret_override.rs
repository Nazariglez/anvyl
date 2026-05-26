use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(setter, ret = "int")]
    pub fn x(&mut self, value: i64) {}
}

fn main() {}
