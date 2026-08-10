use anvyx_runtime::AnvyxRef;

#[derive(AnvyxRef)]
pub struct Token {
    #[anvyx(field)]
    pub value: i64,
}

anvyx_runtime::builtin_module! {
    provider: "type_provider",
    name: "types",
    root: false,
    exports: [Token],
}
