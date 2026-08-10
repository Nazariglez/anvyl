use anvyx_runtime::{function, AnvyxInline};

#[derive(Clone, Copy, AnvyxInline)]
#[anvyx(name = "WindowConfig")]
pub struct RustWindowConfig {
    width: i64,
}

#[derive(Clone, Copy, AnvyxInline)]
#[anvyx(name = "UnderscoreConfig")]
pub struct __ {
    width: i64,
}

#[function(ret = "WindowConfig")]
pub fn make_config(width: i64) -> RustWindowConfig {
    RustWindowConfig { width }
}

#[function(params(config = "WindowConfig"))]
pub fn config_width(config: RustWindowConfig) -> i64 {
    config.width
}

#[function(ret = "UnderscoreConfig")]
pub fn make_underscores(width: i64) -> __ {
    __ { width }
}

#[function(params(config = "UnderscoreConfig"))]
pub fn underscores_width(config: __) -> i64 {
    config.width
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [
        make_config,
        config_width,
        RustWindowConfig,
        make_underscores,
        underscores_width,
        __,
    ],
}
