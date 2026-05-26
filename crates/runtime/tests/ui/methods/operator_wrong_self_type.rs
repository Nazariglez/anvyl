use anvyx_runtime::methods;

struct Point;
struct Other;

#[methods]
impl Point {
    #[anvyx(op(Self + Self))]
    pub fn add(&self, rhs: Other) -> i64 { 1 }
}

fn main() {}
