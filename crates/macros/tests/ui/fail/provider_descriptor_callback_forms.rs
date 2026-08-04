use anvyx_macros::provider_descriptor;

fn main() {
    let _ = provider_descriptor! {
        provider = "host",
        module = "host",
        fn make() -> fn(int);
    };
    let _ = provider_descriptor! {
        provider = "host",
        module = "host",
        fn each(callback: [fn(int)]) -> void;
    };
    let _ = provider_descriptor! {
        provider = "host",
        module = "host",
        fn many(callback: fn(int, int, int, int, int, int, int, int, int)) -> void;
    };
    let _ = provider_descriptor! {
        provider = "host",
        module = "host",
        fn leaves(callback: fn(string) -> [int]) -> void;
    };
    let _ = provider_descriptor! {
        provider = "host",
        module = "host",
        fn returns(callback: fn(int) -> string) -> void;
    };
}
