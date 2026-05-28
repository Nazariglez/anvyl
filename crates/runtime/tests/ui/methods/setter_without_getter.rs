use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(setter)]
    pub fn set_x(&mut self, value: i64) {}
}

fn main() {}
