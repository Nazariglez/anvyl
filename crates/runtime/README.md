# anvyx-runtime provider API

Rust provider crates expose Anvyx extern modules with runtime macros.

Minimal provider crate:

```rust
use anvyx_runtime::function;

#[function]
pub fn host_add(a: i64, b: i64) -> i64 {
    a + b
}

#[function]
pub fn host_len(text: &str) -> i64 {
    text.len() as i64
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_add, host_len],
}
```

Project manifest:

```toml
[externs.host]
path = "provider"
```

Anvyx source:

```anvyx
import ext:host { host_add, host_len };
```

Supported scalar mapping for `#[function]`:

| Anvyx | Rust |
|---|---|
| `void` | `()` |
| `bool` | `bool` |
| `int` | `i64` |
| `float` | `f64` |
| `string` borrowed parameter | `&str` |
| `string` return | `String` |

`Option<T>` returns are supported for scalar `T`. `Vec<T>` and several complex
ABIs are described in metadata but rejected by the clean Rust backend until
wrapper conversion support exists.

Provider crates must export `provider_descriptor()` and `rust_module_support()`;
`builtin_module!` generates both.
