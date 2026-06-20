# anvyx-runtime provider API

Rust provider crates expose Anvyx extern modules with runtime macros.

## Single-module provider crates

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

`builtin_module!` describes one native module. It generates the module-level
helpers and the plural package-probe helpers used by Anvyx:

```rust
provider_descriptor() -> ProviderDescriptor
provider_descriptors() -> Vec<ProviderDescriptor>
rust_module_support() -> RustModuleSupport
rust_module_supports() -> Vec<RustModuleSupport>
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

Root package source imports native-only dependency modules with `pkg:`:

```anvyx
import pkg:host.host { host_add, host_len };
```

Source inside a source+native provider package imports its own provider modules
with `ext:`:

```anvyx
import ext:host { host_add, host_len };
```

## Multi-module provider crates

Use `provider_package!` at the Rust crate root when one provider crate exposes
several Anvyx native modules:

```rust
mod window;
mod gpu;

anvyx_runtime::provider_package! {
    modules: [window, gpu],
}
```

Each listed Rust module uses `builtin_module!` for one Anvyx module:

```rust
// src/window.rs
use anvyx_runtime::function;

#[function]
pub fn open_window() -> i64 {
    1
}

anvyx_runtime::builtin_module! {
    name: "window",
    source: "",
    exports: [open_window],
}
```

```rust
// src/gpu.rs
use anvyx_runtime::function;

#[function]
pub fn create_device() -> i64 {
    2
}

anvyx_runtime::builtin_module! {
    name: "gpu",
    source: "",
    exports: [create_device],
}
```

The module list is explicit. Paths are crate-root-relative Rust module paths;
there is no auto-discovery. Nested paths such as `platform::window` are allowed.
`provider_package!` preserves each child provider id, aggregates child
descriptors/supports, and retargets native wrapper/type paths so private Rust
submodules can still be used by generated package users.

Dependents import the exposed modules with `pkg:`:

```anvyx
import pkg:host.window { open_window };
import pkg:host.gpu { create_device };
```

A source+native provider package can import its own native modules with `ext:`
and re-export source APIs as usual:

```anvyx
pub import ext:window;
pub import ext:gpu;
```

Anvyx probes provider crates through the plural package ABI:

```rust
provider_descriptors() -> Vec<ProviderDescriptor>
rust_module_supports() -> Vec<RustModuleSupport>
```

Do not hand-write descriptor merging for multi-module packages; use
`provider_package!`.

## Function ABI

Supported scalar mapping for `#[function]`:

| Anvyx | Rust |
|---|---|
| `void` | `()` |
| `bool` | `bool` |
| `int` | `i64` |
| `float` | `f64` |
| `string` borrowed parameter | `&str` |
| `string` return | `String` |

`Option<T>` returns are supported for scalar `T`. `Vec<T>` parameters and
returns are supported through wrapper conversion for scalar `T` (`bool`, `i64`,
`f64`, `String`). Direct collection ABI, nested lists, maps, mutable collection
ABI, and list-of-native-struct wrappers remain unsupported.

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
borrowed params, mutable provider params, or `Vec<T>` wrapper conversion in this slice.

```rust
use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function]
pub fn each(f: ScopedLambda<'_, '_, (i64,), ()>) -> Result<(), RuntimeError> {
    f.call(1)?;
    f.call(2)
}
```
