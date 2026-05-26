use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(op(Self + Self), ret = "int")]
    pub fn add(&self, rhs: Point) -> Point { rhs }
}

fn main() {}
