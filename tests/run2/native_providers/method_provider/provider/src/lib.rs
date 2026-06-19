use anvyx_runtime::{function, methods, AnvyxInline};

#[derive(Clone, Copy, AnvyxInline)]
pub struct Vec2 {
    #[anvyx(field)]
    pub x: f64,
    #[anvyx(field)]
    pub y: f64,
}

#[methods]
impl Vec2 {
    #[anvyx(init)]
    pub fn new() -> Self {
        Self { x: 3.0, y: 4.0 }
    }

    pub fn len2(&self) -> f64 {
        self.x * self.x + self.y * self.y
    }

    #[anvyx(op(Self + Self))]
    pub fn add(&self, rhs: Vec2) -> Vec2 {
        Vec2 {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

#[function]
pub fn marker() -> i64 {
    0
}

#[function]
pub fn len2_of(value: Vec2) -> f64 {
    value.x * value.x + value.y * value.y
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [marker, len2_of, Vec2],
}
