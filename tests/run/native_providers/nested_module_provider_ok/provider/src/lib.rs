mod window;
mod platform;

anvyx_runtime::provider_package! { modules: [window, platform::input] }
