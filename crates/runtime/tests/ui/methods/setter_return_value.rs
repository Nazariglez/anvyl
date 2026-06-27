use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(getter)]
    pub fn x(&self) -> i64 {
        1
    }

    #[anvyx(setter)]
    pub fn set_x(&mut self, _value: i64) -> i64 {
        1
    }
}

fn main() {}
