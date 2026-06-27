use anvyx_runtime::methods;

struct Point;

#[methods]
impl Point {
    #[anvyx(getter)]
    pub fn x(&self) -> anvyx_runtime::RuntimeResult<i64> {
        Ok(1)
    }

    #[anvyx(setter)]
    pub fn set_x(&mut self, _value: i64) {}
}

fn main() {}
