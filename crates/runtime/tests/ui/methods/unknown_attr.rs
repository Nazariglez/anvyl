use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(field)]
    pub fn x(&self) -> i64 { 1 }
}

fn main() {}
