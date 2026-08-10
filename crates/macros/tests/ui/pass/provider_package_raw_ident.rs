use anvyx_runtime::function;

mod r#type {
    use super::function;

    #[function]
    pub fn ping() -> i64 { 1 }

    anvyx_runtime::builtin_module! {
        name: "raw",
    root: false,
        exports: [ping],
    }
}

anvyx_runtime::provider_package! { modules: [r#type] }

fn main() {}
