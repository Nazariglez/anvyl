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

Provider package `anvyx.toml`:

```toml
[project]
name = "host"
```

Provider package `Cargo.toml`:

```toml
[package]
name = "host-native"
version = "0.1.0"
edition = "2024"

[dependencies]
anvyx-runtime = { path = "/path/to/anvyx/crates/runtime" }
```

Adjust the path for your workspace layout.

Root package manifest:

```toml
[dependencies]
host = { path = "../host" }
```

Anvyx source in the root package imports native-only dependency modules with `pkg:`:

```anvyx
import pkg:host.host { host_add, host_len };
```

Anvyx source inside a source+native provider package imports its own provider modules with `ext:`:

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

Mutable provider parameters have two Rust ABI shapes:

- `&mut T` is a low-level/manual ABI. The clean Rust backend only passes whole
  mutable local lvalues to it.
- `#[function(ctx)]` with `MutPlace<'_, 'cx, T>` is the normal place-aware ABI.
  It accepts supported projected/source mutable places without copy-back. The
  macro currently supports `bool`, `i64`, `f64`, and `Option` of those payloads;
  provider authors needing runtime payloads such as strings or lists must use
  manual metadata with the runtime representation type.

```rust
use anvyx_runtime::{function, Ctx, MutPlace, RuntimeError};

#[function(ctx)]
pub fn bump<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    mut value: MutPlace<'_, 'cx, i64>,
) -> Result<(), RuntimeError> {
    value.update_copy(ctx, |n| n + 1)
}
```

Use `MutPlace` only during the call. Access or mutate it through the provided
short closures and do not store it. `#[methods] fn method(&mut self, ...)` stays
a direct mutable receiver ABI; manual/final provider metadata may use
`RustParamAbi::MutPlace(owner)` as receiver parameter 0 for place-aware receiver
bindings.

Provider functions can accept non-escaping Anvyx function values with
`ScopedLambda<'_, '_, Args, Ret>`. `Args` is a tuple of up to 8 supported
callback ABI leaves (`bool`, `i64`, `f64`); `Ret` may also be `()`. Call it
synchronously and return or handle `RuntimeError`; do not store it.
`ScopedLambda` cannot be combined with `#[function(ctx)]`, method receivers,
borrowed params, or mutable provider params in this slice.

```rust
use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function]
pub fn each(f: ScopedLambda<'_, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    f.call(1)?;
    f.call(2)
}
```

Provider crates must export `provider_descriptor()` and `rust_module_support()`;
`builtin_module!` generates both.
