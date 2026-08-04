anvyx_runtime::provider_package! {}
anvyx_runtime::provider_package! { exports: [window] }
anvyx_runtime::provider_package! { modules: [] }
anvyx_runtime::provider_package! { modules: [window, window] }
anvyx_runtime::provider_package! { modules: [crate::window] }
anvyx_runtime::provider_package! { modules: [self::window] }
anvyx_runtime::provider_package! { modules: [super::window] }
anvyx_runtime::provider_package! { modules: [window::<i64>] }

fn main() {}
