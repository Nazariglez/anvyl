# Test runner

`test-runner` executes Anvyx fixture files from `tests/`. It parses `// @...` directives at the top of each `.anv` file, runs the production CLI, classifies the result, and prints a human or JSON report.

## Usage

```bash
cargo run --package test-runner -- [OPTIONS] <PATH>...
```

Common examples:

```bash
cargo run --package test-runner -- tests --quiet
cargo run --package test-runner -- tests/syntax --backend vm
cargo run --package test-runner -- tests/run --backend both --jobs 8
cargo run --package test-runner -- tests --report-json
cargo run --package test-runner -- tests/syntax --new-frontend --quiet
```

Options:

| Option | Meaning |
| --- | --- |
| `--backend <vm|rust|both>` | Backend to test. Defaults to `vm`. |
| `--new-frontend` | Route check-mode fixtures through `anvyx check --new-frontend`; run-mode fixtures are skipped. |
| `--timeout <ms>` | Runtime timeout. Defaults to `2000`. |
| `--compile-timeout <ms>` | Compile timeout. Defaults to `300000`. |
| `--jobs <n>` | Maximum parallel tests. Defaults to Rayon. |
| `--quiet` | Hide per-test output. |
| `--report-json` | Emit a JSON report. |
| `--release` | Build Anvyx in release mode. |

Options can appear before or after paths. Repeated value options use the last value.

By default, fixtures run through the production CLI default frontend/backend path. `--new-frontend` uses the same compiled CLI binary and adds `--new-frontend` to check-mode child invocations. It is check-only, so run-mode fixtures are skipped before spawning `anvyx`. Use `// @frontend: new` or `// @frontend: default` for fixtures that only apply to one frontend path.

## Fixture directives

Directives must appear in leading line comments. Parsing stops at the first non-comment line.

Most tests require both `@mode` and `@expect`:

```anvyx
// @mode: run
// @expect: success
// @match: hello

fn main() {
    println("hello");
}
```

Helper files are different. A helper file must contain only `// @helper` as its directive.

### Directives

| Directive | Repeat | Value | Meaning |
| --- | --- | --- | --- |
| `// @mode: check` | once | `check` or `run` | Compile only or compile and execute. |
| `// @expect: success` | once | `success`, `error`, or `timeout` | Expected outcome. |
| `// @exit-code: 7` | once | `0..255` | Expected successful process exit code. Valid only with `@mode: run` and `@expect: success`. |
| `// @match: text` | once | exact line | Exact selected-output match. |
| `// @match-begin` | once | none | Starts a multi-line exact selected-output block. |
| `// @match-end` | once | none | Ends a multi-line exact selected-output block. |
| `// @contains: text` | many | substring | Selected output must contain this substring on some line. |
| `// @stderr-match: text` | once | exact line | Exact stderr match. |
| `// @stderr-contains: text` | many | substring | Stderr must contain this substring on some line. |
| `// @stdin: text` | many | line | Adds one stdin line. Valid only in `run` mode. |
| `// @stdin-empty-line` | many | none | Adds one blank stdin line. Valid only in `run` mode. |
| `// @warn-contains: text` | many | substring | Successful test stderr must contain this warning substring. Valid only with `@expect: success`. |
| `// @frontend: any\|default\|new` | once | frontend | Runs only on the matching frontend path. `any` is the default. Non-matching fixtures are reported as skipped. |
| `// @skip: reason` | once | reason | Skips the test and reports the reason. |
| `// @helper` | once | none | Marks a helper module. Cannot be combined with other directives. |
| `// @lint: value` | many | lint override | Forwards `--lint <value>` to the child CLI. |
| `// @feature: value` | many | feature | Forwards `--feature <value>` to the child CLI. |
| `// @cfg: value` | many | cfg | Forwards `--cfg <value>` to the child CLI. |

### Selected output

`@match`, `@match-begin`, and `@contains` check the selected output stream:

| Case | Selected output |
| --- | --- |
| `run` success | `stdout` |
| `run` error | `stderr` |
| `check` success or error | `stdout` and `stderr` concatenated |

`@stderr-match` and `@stderr-contains` always check `stderr`.

### Matching rules

Rules:

- `@match` conflicts with `@match-begin`.
- `@contains` conflicts with exact selected-output matching.
- `@stderr-contains` conflicts with `@stderr-match`.
- `@match-begin` blocks must end with `@match-end`.
- A `@match-begin` block must contain at least two comment lines.
- Non-comment lines inside a `@match-begin` block are invalid.

Example multi-line match:

```anvyx
// @mode: run
// @expect: success
// @match-begin
// first line
// second line
// @match-end
```

## Adding a directive

The directive registry lives in `src/directives.rs`. Add normal directives there first, then update only the downstream owner that needs the new semantic data.

Typical steps:

1. Add a `DirectiveKind` variant.
2. Add one row to `DIRECTIVE_SPECS` with the directive name, value rule, and repeatability.
3. Extend `Directives::apply_directive` to map the parsed value into the correct semantic group.
4. Extend `Directives::validate` only if the directive has cross-field rules.
5. Update downstream code only if the directive changes execution, assertion, classification, or reporting behavior.
6. Add parser and behavior tests.
7. Update this README.

For forwarded CLI flags, prefer `CliFlag` plus one `DIRECTIVE_SPECS` row. Do not add a new `Directives` field or a custom loop in `run_test/cli.rs` unless the directive has behavior beyond forwarding a CLI flag.

Relevant files:

- `src/directives.rs`: directive parsing, validation, and semantic grouping
- `src/run_test/assertions.rs`: output assertion checks
- `src/run_test/classifier.rs`: process outcome classification
- `src/run_test/cli.rs`: child CLI process construction and timeout handling
- `src/report.rs`: human and JSON report aggregation
- `src/args.rs`: test-runner CLI arguments
