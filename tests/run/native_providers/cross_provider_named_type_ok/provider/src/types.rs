use anvyx_runtime::{AnvyxInline, methods};

#[derive(Clone, Copy, AnvyxInline)]
pub struct Token {
    #[anvyx(field)]
    pub value: i64,
}

#[methods]
impl Token {
    #[anvyx(init)]
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

anvyx_runtime::builtin_module! {
    provider: "type_provider",
    name: "types",
    root: false,
    exports: [Token],
}
