run target:
    cargo run -- run {{target}}

check target:
    cargo run -- check {{target}}

tests: frontend-tests clean-rust-tests

frontend-tests target="tests/syntax":
    cargo run --package test-runner -- {{target}} --new-frontend --quiet

clean-rust-tests target="tests/run2":
    cargo run --package test-runner -- {{target}} --new-frontend --backend rust --quiet

full-tests target="tests":
    cargo test -q --workspace
    cargo run --package test-runner -- {{target}} --quiet

full-tests-release target="tests":
    cargo test -q --workspace --release
    cargo run --package test-runner -- {{target}} --release

install:
    cargo install --path crates/anvyx --force

miri:
    MIRIFLAGS="-Zmiri-strict-provenance" cargo +nightly miri test -p anvyx-lang --all-targets

clean-rust-cache:
    rm -rf .anvyx/cache/rust/artifacts

scan-tests threshold="75":
    python3 ./scripts/scan_tests.py -t {{threshold}}

fmt:
    cargo +nightly fmt

# keep editor grammars in sync with lexer keywords
check-editor-keywords:
    bash editors/scripts/check_keywords.sh
