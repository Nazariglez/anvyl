use anvyx_runtime::function;

use crate::types::Token;

#[function]
pub fn consume(token: Token) -> i64 {
    token.value
}

anvyx_runtime::builtin_module! {
    provider: "call_provider",
    name: "calls",
    root: false,
    exports: [consume],
}
