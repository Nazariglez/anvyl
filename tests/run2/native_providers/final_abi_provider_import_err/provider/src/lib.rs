use anvyx_runtime::function;

pub struct Secret {
    pub value: i64,
}

#[function]
pub fn secret_value(secret: Secret) -> i64 {
    secret.value
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [secret_value],
}
