use anvyx_runtime::{function, methods, AnvyxInline};

#[derive(Clone, Copy, AnvyxInline)]
pub struct Vec2 {
    x: f64,
    y: f64,
}

#[methods]
impl Vec2 {
    #[anvyx(init)]
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    #[anvyx(getter)]
    pub fn x(&self) -> f64 {
        self.x
    }

    #[anvyx(setter)]
    pub fn set_x(&mut self, value: f64) {
        self.x = value;
    }

    #[anvyx(getter)]
    pub fn y(&self) -> f64 {
        self.y
    }

    #[anvyx(setter)]
    pub fn set_y(&mut self, value: f64) {
        self.y = value;
    }

    #[anvyx(getter)]
    pub fn sum(&self) -> f64 {
        self.x + self.y
    }

    #[anvyx(setter)]
    pub fn set_sum(&mut self, value: f64) {
        self.x = value - self.y;
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

#[derive(Clone, Copy, AnvyxInline)]
pub struct Camera {
    fov: f64,
}

#[methods]
impl Camera {
    #[anvyx(init)]
    pub fn new() -> Self {
        Self { fov: 45.0 }
    }

    #[anvyx(getter)]
    pub fn fov(&self) -> f64 {
        self.fov
    }

    #[anvyx(setter)]
    pub fn set_fov(&mut self, value: f64) {
        self.fov = value;
    }
}

#[function]
pub fn len2_of(value: Vec2) -> f64 {
    value.x * value.x + value.y * value.y
}

#[function]
pub fn fov_of(camera: Camera) -> f64 {
    camera.fov
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [len2_of, fov_of, Vec2, Camera],
}
