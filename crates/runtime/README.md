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
| owned/value `string` | `AnvString` |

`String` is not a provider boundary carrier. Use `AnvString` for owned string
values or `&str` for borrowed parameters. `Vec<T>`, `HashMap<K, V>`, and
`BTreeMap<K, V>` are not provider boundary carriers; use `AnvList`, `AnvMap`,
or `AnvSlice` plus explicit copy helpers when allocation is intentional.

Return `RuntimeResult<T>` for hidden runtime/provider failure. Visible
`Result<T, E>` remains an ordinary Anvyx result value. Bare
`Result<T, RuntimeError>` is rejected to avoid two hidden-failure spellings.

For fixed arrays, Rust `[T; N]` is the direct carrier for Anvyx `[T; N]`.
Element conversion is recursive through the same ABI rules. Fixed arrays are
value-shaped, size-known values and do not use runtime collection carriers.

For `rep shared` resources, returning owned `T`, `Option<T>`, or visible
`Result<T, E>` transfers the resource into the Anvyx runtime. Returning
`AnvRef<'cx, T>` means the value is already managed and is not adopted again.
Owned resources inside tuple, fixed-array, list, or map returns are rejected
until the ABI carries ownership metadata for each leaf. Structural returns
containing `AnvRef<'cx, T>` are rejected for the same reason; use top-level,
`Option`, or visible `Result` resource returns. Provider parameters for shared
resources must use `AnvRef<'cx, T>` directly or through `Option` / visible
`Result`; bare `T` and structural tuple, fixed-array, list, or map parameters
containing shared resources are rejected.

Mutable provider parameters have two Rust ABI shapes:

- `&mut T` is a low-level/manual ABI. The clean Rust backend only passes whole
  mutable local lvalues to it.
- `#[function(ctx)]` with `MutPlace<'_, 'cx, T>` is the normal place-aware ABI.
  It accepts supported projected/source mutable places without copy-back. The
  macro currently supports `bool`, `i64`, `f64`, and `Option` of those payloads;
  provider authors needing runtime payloads such as strings or lists must use
  manual metadata with the runtime representation type.

```rust
use anvyx_runtime::{function, Ctx, MutPlace, RuntimeResult};

#[function(ctx)]
pub fn bump<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    mut value: MutPlace<'_, 'cx, i64>,
) -> RuntimeResult<()> {
    value.update_copy(ctx, |n| n + 1)
}
```

Use `MutPlace` only during the call. Access or mutate it through the provided
short closures and do not store it. `#[methods] fn method(&mut self, ...)` stays
a direct mutable receiver ABI; manual/final provider metadata may use
`RustParamAbi::MutPlace(owner)` as receiver parameter 0 for place-aware receiver
bindings.

`#[anvyx(init)]` parameters may use `AnvInitField<T>` when a provider-backed
extern literal should distinguish a provided field from an omitted field:

```rust
use anvyx_runtime::{methods, AnvInitField};

pub struct Camera { fov: Option<i64> }

#[methods]
impl Camera {
    #[anvyx(init)]
    pub fn new(fov: AnvInitField<Option<i64>>) -> Camera {
        Camera {
            fov: match fov {
                AnvInitField::Provided(value) => value,
                AnvInitField::Omitted => None,
            },
        }
    }
}
```

Provider functions can accept non-escaping Anvyx function values with
`ScopedLambda<'_, '_, Args, Ret>`. `Args` is a tuple of up to 8 supported
callback ABI leaves (`bool`, `i64`, `f64`); `Ret` may also be `()`. Call it
synchronously and return or handle `RuntimeError`; do not store it. Return
`RuntimeResult<T>` when callback failure should be hidden runtime failure.
`ScopedLambda` cannot be combined with `#[function(ctx)]`, method receivers,
borrowed params, or mutable provider params in this slice.

Provider-retained callbacks outside heap resources use `EscapingLambda<Args, Ret>`
leases. They are same-thread runtime leases and must not be stored inside
`AnvyxRef` payloads. Heap-stored resource fields use `AnvCallback<'cx, Args, Ret>`;
invoke them only after any receiver/resource borrow has ended. Resources that
store `AnvCallback`, `AnvRef`, or other heap edges must be tracked and traced.

```rust
use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function]
pub fn each(f: ScopedLambda<'_, '_, (i64,), ()>) -> RuntimeResult<()> {
    f.call(1)?;
    f.call(2)
}
```
