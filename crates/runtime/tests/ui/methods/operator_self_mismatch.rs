use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(op(Self + Self))]
    pub fn add(&self, rhs: i64) -> i64 { rhs }
}

fn main() {}
