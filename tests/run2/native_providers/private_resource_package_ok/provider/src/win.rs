use anvyx_runtime::{function, methods, AnvyxRef};

#[derive(AnvyxRef)]
pub struct WWin {
    #[anvyx(field)]
    value: i64,
    score: i64,
}

#[methods]
impl WWin {
    #[anvyx(init)]
    pub fn new(value: i64) -> WWin {
        WWin {
            value,
            score: value * 2,
        }
    }

    pub fn duplicate(&self, delta: i64) -> WWin {
        WWin {
            value: self.value + delta,
            score: self.score + delta,
        }
    }

    #[anvyx(getter)]
    pub fn score(&self) -> i64 {
        self.score
    }

    #[anvyx(setter)]
    pub fn set_score(&mut self, value: i64) {
        self.score = value;
    }
}

#[function]
pub fn make_win(value: i64) -> WWin {
    WWin {
        value,
        score: value * 3,
    }
}

anvyx_runtime::builtin_module! {
    name: "win",
    source: "",
    exports: [WWin, make_win],
}
