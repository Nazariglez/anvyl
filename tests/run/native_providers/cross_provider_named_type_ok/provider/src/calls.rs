use anvyx_runtime::function;

use crate::types::Token;

#[function]
pub fn increment(token: Token) -> i64 {
    token.value + 1
}

anvyx_runtime::builtin_module! {
    provider: "call_provider",
    name: "calls",
    root: false,
    exports: [increment],
}
