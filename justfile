run target:
    cargo run -- run {{target}}

check target:
    cargo run -- check {{target}}

tests target="tests":
    cargo run --package test-runner -- {{target}}

tests-release target="tests":
    cargo run --package test-runner -- {{target}} --release