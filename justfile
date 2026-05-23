run target:
    cargo run -- run {{target}}

check target:
    cargo run -- check {{target}}

tests target="tests":
    cargo run --package test-runner -- {{target}} --quiet

frontend-tests target="tests/syntax":
    cargo run --package test-runner -- {{target}} --new-frontend --quiet

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

rust-coverage floor="878":
    python3 ./scripts/rust_coverage.py --floor {{floor}}

clean-rust-cache:
    rm -rf .anvyx/cache/rust/artifacts

scan-tests threshold="75":
    python3 ./scripts/scan_tests.py -t {{threshold}}

fmt:
    cargo +nightly fmt

# keep editor grammars in sync with lexer keywords
check-editor-keywords:
    bash editors/scripts/check_keywords.sh
