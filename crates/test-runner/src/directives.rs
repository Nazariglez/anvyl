use crate::model::{ExpectedResult, Mode};

#[derive(Debug, Default, Clone)]
pub(crate) struct Directives {
    pub(crate) contract: TestContract,
    pub(crate) assertions: Assertions,
    pub(crate) stdin: Stdin,
    pub(crate) cli_options: CliOptions,
    // FIXME: remove frontend once it promoted to the only frtonend
    pub(crate) frontend: FrontendRequirement,
    pub(crate) skip: Option<String>,
    pub(crate) helper: bool,
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct TestContract {
    pub(crate) mode: Mode,
    pub(crate) expect: ExpectedResult,
    pub(crate) exit_code: Option<u8>,
}

impl TestContract {
    pub(crate) fn success_exit_code(self) -> i32 {
        self.user_exit_code().unwrap_or(0)
    }

    pub(crate) fn user_exit_code(self) -> Option<i32> {
        match self.mode {
            Mode::Run => self.exit_code.map(i32::from),
            Mode::Check => None,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FrontendRequirement {
    #[default]
    Any,
    Default,
    New,
}

impl FrontendRequirement {
    fn from_str(value: &str) -> Result<Self, String> {
        match value {
            "any" => Ok(Self::Any),
            "default" => Ok(Self::Default),
            "new" => Ok(Self::New),
            _ => Err(format!("invalid @frontend value: {value}")),
        }
    }

    pub(crate) fn skip_reason(self, new_frontend: bool) -> Option<&'static str> {
        match (self, new_frontend) {
            (Self::Any, _) | (Self::Default, false) | (Self::New, true) => None,
            (Self::Default, true) => Some("requires default frontend"),
            (Self::New, false) => Some("requires new frontend"),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Assertions {
    pub(crate) selected: StreamAssertions,
    pub(crate) stderr: StreamAssertions,
    pub(crate) warnings: ContainsAssertions,
}

#[derive(Debug, Default, Clone)]
pub(crate) struct StreamAssertions {
    pub(crate) exact: Option<String>,
    pub(crate) contains: Vec<String>,
}

impl StreamAssertions {
    fn has_conflict(&self) -> bool {
        self.exact.is_some() && !self.contains.is_empty()
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct ContainsAssertions {
    pub(crate) contains: Vec<String>,
}

impl ContainsAssertions {
    pub(crate) fn is_empty(&self) -> bool {
        self.contains.is_empty()
    }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Stdin {
    lines: Vec<String>,
}

impl Stdin {
    pub(crate) fn push(&mut self, line: String) {
        self.lines.push(line);
    }

    pub(crate) fn text(&self) -> String {
        if self.lines.is_empty() {
            return String::new();
        }

        format!("{}\n", self.lines.join("\n"))
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CliFlag {
    Lint,
    Feature,
    Cfg,
}

impl CliFlag {
    pub(crate) fn cli_flag(self) -> &'static str {
        match self {
            Self::Lint => "--lint",
            Self::Feature => "--feature",
            Self::Cfg => "--cfg",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ForwardedCliArg {
    flag: CliFlag,
    value: String,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct CliOptions {
    forwarded: Vec<ForwardedCliArg>,
}

impl CliOptions {
    pub(crate) fn push(&mut self, flag: CliFlag, value: String) {
        self.forwarded.push(ForwardedCliArg { flag, value });
    }

    pub(crate) fn append_args(&self, args: &mut Vec<String>) {
        for arg in &self.forwarded {
            args.extend([arg.flag.cli_flag().to_string(), arg.value.clone()]);
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DirectiveKind {
    Mode,
    Expect,
    ExitCode,
    Match,
    MatchBegin,
    MatchEnd,
    Contains,
    StderrMatch,
    StderrContains,
    Stdin,
    StdinEmptyLine,
    WarnContains,
    Frontend,
    Skip,
    Helper,
    CliOption(CliFlag),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueRule {
    None,
    Required(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Repeatability {
    Once,
    Many,
}

#[derive(Debug, Clone, Copy)]
struct DirectiveSpec {
    name: &'static str,
    kind: DirectiveKind,
    value: ValueRule,
    repeat: Repeatability,
}

impl DirectiveSpec {
    const fn flag(name: &'static str, kind: DirectiveKind, repeat: Repeatability) -> Self {
        Self {
            name,
            kind,
            value: ValueRule::None,
            repeat,
        }
    }

    const fn value(
        name: &'static str,
        kind: DirectiveKind,
        metavar: &'static str,
        repeat: Repeatability,
    ) -> Self {
        Self {
            name,
            kind,
            value: ValueRule::Required(metavar),
            repeat,
        }
    }
}

const DIRECTIVE_SPECS: &[DirectiveSpec] = &[
    DirectiveSpec::value(
        "mode",
        DirectiveKind::Mode,
        "check|run",
        Repeatability::Once,
    ),
    DirectiveSpec::value(
        "expect",
        DirectiveKind::Expect,
        "success|error|timeout",
        Repeatability::Once,
    ),
    DirectiveSpec::value(
        "exit-code",
        DirectiveKind::ExitCode,
        "0..255",
        Repeatability::Once,
    ),
    DirectiveSpec::value(
        "match",
        DirectiveKind::Match,
        "exact line",
        Repeatability::Once,
    ),
    DirectiveSpec::flag(
        "match-begin",
        DirectiveKind::MatchBegin,
        Repeatability::Once,
    ),
    DirectiveSpec::flag("match-end", DirectiveKind::MatchEnd, Repeatability::Once),
    DirectiveSpec::value(
        "contains",
        DirectiveKind::Contains,
        "substring",
        Repeatability::Many,
    ),
    DirectiveSpec::value(
        "stderr-match",
        DirectiveKind::StderrMatch,
        "exact line",
        Repeatability::Once,
    ),
    DirectiveSpec::value(
        "stderr-contains",
        DirectiveKind::StderrContains,
        "substring",
        Repeatability::Many,
    ),
    DirectiveSpec::value("stdin", DirectiveKind::Stdin, "line", Repeatability::Many),
    DirectiveSpec::flag(
        "stdin-empty-line",
        DirectiveKind::StdinEmptyLine,
        Repeatability::Many,
    ),
    DirectiveSpec::value(
        "warn-contains",
        DirectiveKind::WarnContains,
        "substring",
        Repeatability::Many,
    ),
    DirectiveSpec::value(
        "frontend",
        DirectiveKind::Frontend,
        "any|default|new",
        Repeatability::Once,
    ),
    DirectiveSpec::value("skip", DirectiveKind::Skip, "reason", Repeatability::Once),
    DirectiveSpec::flag("helper", DirectiveKind::Helper, Repeatability::Once),
    DirectiveSpec::value(
        "lint",
        DirectiveKind::CliOption(CliFlag::Lint),
        "override",
        Repeatability::Many,
    ),
    DirectiveSpec::value(
        "feature",
        DirectiveKind::CliOption(CliFlag::Feature),
        "feature",
        Repeatability::Many,
    ),
    DirectiveSpec::value(
        "cfg",
        DirectiveKind::CliOption(CliFlag::Cfg),
        "cfg",
        Repeatability::Many,
    ),
];

#[derive(Debug)]
struct ParsedDirective<'a> {
    name: &'a str,
    value: Option<&'a str>,
}

#[derive(Debug, Default)]
struct SeenDirectives {
    kinds: Vec<DirectiveKind>,
}

impl SeenDirectives {
    fn record(&mut self, spec: &DirectiveSpec) -> Result<(), String> {
        if spec.repeat == Repeatability::Once && self.has(spec.kind) {
            return Err(format!("duplicate @{} directive", spec.name));
        }
        self.kinds.push(spec.kind);
        Ok(())
    }

    fn has(&self, kind: DirectiveKind) -> bool {
        self.kinds.contains(&kind)
    }

    fn only_has(&self, kind: DirectiveKind) -> bool {
        self.kinds.iter().all(|seen| *seen == kind)
    }
}

impl Directives {
    pub fn parse(src: &str) -> Result<Self, String> {
        let mut directives = Self::default();
        let mut seen = SeenDirectives::default();
        let mut match_block: Option<Vec<String>> = None;

        for line in src.lines() {
            let trimmed = line.trim();

            if let Some(lines) = &mut match_block {
                match trimmed {
                    "// @match-end" => {
                        seen.record(spec_for("match-end").expect("@match-end spec"))?;
                        if lines.len() < 2 {
                            return Err(
                                "@match-begin block requires at least two lines".to_string()
                            );
                        }
                        directives.assertions.selected.exact = Some(lines.join("\n"));
                        match_block = None;
                    }
                    "// @match-begin" => {
                        return Err("nested @match-begin directive".to_string());
                    }
                    "//" => lines.push(String::new()),
                    _ if trimmed.starts_with("// ") => {
                        let line = trimmed.strip_prefix("// ").expect("comment line prefix");
                        lines.push(line.to_string());
                    }
                    _ if trimmed.starts_with("//") => {}
                    _ => return Err("non-comment line inside @match-begin block".to_string()),
                }
                continue;
            }

            if !trimmed.starts_with("//") {
                break;
            }

            let Some(parsed) = parse_directive_line(trimmed) else {
                continue;
            };
            let spec = spec_for(parsed.name)
                .ok_or_else(|| format!("unknown directive @{}", parsed.name))?;
            let value = validate_directive_value(spec, parsed.value)?;
            seen.record(spec)?;

            match spec.kind {
                DirectiveKind::MatchBegin => match_block = Some(vec![]),
                DirectiveKind::MatchEnd => {
                    return Err("@match-end without @match-begin".to_string());
                }
                _ => directives.apply_directive(spec.kind, value)?,
            }
        }

        if match_block.is_some() {
            return Err("unclosed @match-begin block".to_string());
        }

        directives.validate(&seen)?;

        Ok(directives)
    }

    fn validate(&self, seen: &SeenDirectives) -> Result<(), String> {
        if self.helper {
            if seen.only_has(DirectiveKind::Helper) {
                return Ok(());
            }
            return Err("@helper cannot be combined with other directives".to_string());
        }

        if !seen.has(DirectiveKind::Mode) {
            return Err("missing @mode directive".to_string());
        }
        if !seen.has(DirectiveKind::Expect) {
            return Err("missing @expect directive".to_string());
        }
        if self.contract.exit_code.is_some() && self.contract.mode == Mode::Check {
            return Err("@exit-code is only valid in @mode: run".to_string());
        }
        if self.contract.exit_code.is_some() && self.contract.expect != ExpectedResult::Success {
            return Err("@exit-code requires @expect: success".to_string());
        }
        if !self.stdin.is_empty() && self.contract.mode == Mode::Check {
            return Err("@stdin is only valid in @mode: run".to_string());
        }
        if !self.assertions.warnings.is_empty() && self.contract.expect != ExpectedResult::Success {
            return Err("@warn-contains requires @expect: success".to_string());
        }
        if seen.has(DirectiveKind::Match) && seen.has(DirectiveKind::MatchBegin) {
            return Err("@match conflicts with @match-begin".to_string());
        }
        if self.assertions.selected.has_conflict() {
            return Err("@contains conflicts with exact output matching".to_string());
        }
        if self.assertions.stderr.has_conflict() {
            return Err("@stderr-contains conflicts with @stderr-match".to_string());
        }

        Ok(())
    }

    fn apply_directive(&mut self, kind: DirectiveKind, value: &str) -> Result<(), String> {
        match kind {
            DirectiveKind::Mode => self.contract.mode = Mode::from_str(value)?,
            DirectiveKind::Expect => self.contract.expect = ExpectedResult::from_str(value)?,
            DirectiveKind::ExitCode => {
                self.contract.exit_code = Some(
                    value
                        .parse()
                        .map_err(|_| format!("invalid @exit-code value: {value}"))?,
                );
            }
            DirectiveKind::Match => self.assertions.selected.exact = Some(value.to_string()),
            DirectiveKind::Contains => self.assertions.selected.contains.push(value.to_string()),
            DirectiveKind::StderrMatch => self.assertions.stderr.exact = Some(value.to_string()),
            DirectiveKind::StderrContains => {
                self.assertions.stderr.contains.push(value.to_string());
            }
            DirectiveKind::Stdin => self.stdin.push(value.to_string()),
            DirectiveKind::StdinEmptyLine => self.stdin.push(String::new()),
            DirectiveKind::WarnContains => {
                self.assertions.warnings.contains.push(value.to_string());
            }
            DirectiveKind::Frontend => self.frontend = FrontendRequirement::from_str(value)?,
            DirectiveKind::Skip => self.skip = Some(value.to_string()),
            DirectiveKind::Helper => self.helper = true,
            DirectiveKind::CliOption(flag) => self.cli_options.push(flag, value.to_string()),
            DirectiveKind::MatchBegin | DirectiveKind::MatchEnd => {
                unreachable!("match block directives are handled earlier")
            }
        }
        Ok(())
    }
}

fn parse_directive_line(trimmed: &str) -> Option<ParsedDirective<'_>> {
    let body = trimmed.strip_prefix("// @")?;
    let (name, value) = match body.split_once(':') {
        Some((name, value)) => (name.trim(), Some(value.trim())),
        None => (body.trim(), None),
    };
    Some(ParsedDirective { name, value })
}

fn spec_for(name: &str) -> Option<&'static DirectiveSpec> {
    DIRECTIVE_SPECS.iter().find(|spec| spec.name == name)
}

fn validate_directive_value<'a>(
    spec: &DirectiveSpec,
    value: Option<&'a str>,
) -> Result<&'a str, String> {
    match (spec.value, value) {
        (ValueRule::None, None) => Ok(""),
        (ValueRule::None, Some(_)) => Err(format!("@{} does not take a value", spec.name)),
        (ValueRule::Required(metavar), None | Some("")) => {
            Err(format!("@{} requires a value: {metavar}", spec.name))
        }
        (ValueRule::Required(_), Some(value)) => Ok(value),
    }
}

#[cfg(test)]
mod tests {
    use super::{CliFlag, Directives, FrontendRequirement};

    fn fixture(body: &str) -> String {
        format!("// @mode: run\n// @expect: success\n{body}")
    }

    fn check_fixture(body: &str) -> String {
        format!("// @mode: check\n// @expect: success\n{body}")
    }

    fn directives(src: &str) -> Directives {
        Directives::parse(&fixture(src)).expect("directives should parse")
    }

    #[test]
    fn parses_stderr_directives() {
        let directives = directives(
            "// @stderr-contains: first\n\
             // @stderr-contains: second\n\
             fn main() {}\n",
        );

        assert_eq!(
            directives.assertions.stderr.contains,
            vec!["first", "second"]
        );
    }

    #[test]
    fn parses_stderr_match_directive() {
        let directives = directives("// @stderr-match: exact stderr\nfn main() {}\n");

        assert_eq!(
            directives.assertions.stderr.exact.as_deref(),
            Some("exact stderr")
        );
    }

    #[test]
    fn stdin_directives_build_trailing_newline_text() {
        let directives = directives(
            "// @stdin: first\n\
             // @stdin-empty-line\n\
             // @stdin: third\n\
             fn main() {}\n",
        );

        assert_eq!(directives.stdin.text(), "first\n\nthird\n");
    }

    #[test]
    fn no_stdin_directives_build_empty_text_for_eof() {
        let directives = directives("fn main() {}\n");

        assert!(directives.stdin.is_empty());
        assert_eq!(directives.stdin.text(), "");
    }

    #[test]
    fn parses_exit_code() {
        let directives = directives("// @exit-code: 7\nfn main() {}\n");

        assert_eq!(directives.contract.exit_code, Some(7));
    }

    #[test]
    fn rejects_exit_code_in_check_mode() {
        let error = Directives::parse(&check_fixture("// @exit-code: 7\nfn main() {}\n"))
            .expect_err("@exit-code should be rejected in check mode");

        assert_eq!(error, "@exit-code is only valid in @mode: run");
    }

    #[test]
    fn rejects_duplicate_exit_code() {
        let error = Directives::parse(&fixture(
            "// @exit-code: 1\n// @exit-code: 2\nfn main() {}\n",
        ))
        .expect_err("duplicate @exit-code should fail");

        assert_eq!(error, "duplicate @exit-code directive");
    }

    #[test]
    fn rejects_invalid_exit_code() {
        let error = Directives::parse(&fixture("// @exit-code: 999\nfn main() {}\n"))
            .expect_err("out of range exit code should fail");

        assert_eq!(error, "invalid @exit-code value: 999");
    }

    #[test]
    fn rejects_exit_code_without_success_expectation() {
        let error =
            Directives::parse("// @mode: run\n// @expect: error\n// @exit-code: 7\nfn main() {}\n")
                .expect_err("@exit-code should require success");

        assert_eq!(error, "@exit-code requires @expect: success");
    }

    #[test]
    fn rejects_invalid_mode() {
        let error = Directives::parse("// @mode: nope\nfn main() {}\n")
            .expect_err("invalid mode should fail");

        assert_eq!(error, "Invalid mode: nope");
    }

    #[test]
    fn rejects_invalid_expected_result() {
        let error = Directives::parse("// @expect: nope\nfn main() {}\n")
            .expect_err("invalid expected result should fail");

        assert_eq!(error, "Invalid expected result: nope");
    }

    #[test]
    fn match_block_preserves_empty_comment_lines() {
        let directives = directives(
            "// @match-begin\n\
             // A\n\
             //\n\
             // B\n\
             // @match-end\n\
             fn main() {}\n",
        );

        assert_eq!(
            directives.assertions.selected.exact.as_deref(),
            Some("A\n\nB")
        );
    }

    #[test]
    fn skip_preserves_multi_word_reason() {
        let directives = directives("// @skip: needs old backend behavior\nfn main() {}\n");

        assert_eq!(
            directives.skip.as_deref(),
            Some("needs old backend behavior")
        );
    }

    #[test]
    fn parses_frontend_requirement() {
        let directives = directives("// @frontend: new\nfn main() {}\n");

        assert_eq!(directives.frontend, FrontendRequirement::New);
    }

    #[test]
    fn rejects_invalid_frontend_requirement() {
        let error = Directives::parse(&fixture("// @frontend: maybe\nfn main() {}\n"))
            .expect_err("invalid frontend requirement should fail");

        assert_eq!(error, "invalid @frontend value: maybe");
    }

    #[test]
    fn rejects_duplicate_frontend_requirement() {
        let error = Directives::parse(&fixture(
            "// @frontend: new\n// @frontend: default\nfn main() {}\n",
        ))
        .expect_err("duplicate frontend requirement should fail");

        assert_eq!(error, "duplicate @frontend directive");
    }

    #[test]
    fn rejects_unknown_directive() {
        let error = Directives::parse("// @unknown: value\nfn main() {}\n")
            .expect_err("unknown directive should fail");

        assert_eq!(error, "unknown directive @unknown");
    }

    #[test]
    fn rejects_missing_required_value() {
        let error =
            Directives::parse("// @mode\nfn main() {}\n").expect_err("missing value should fail");

        assert_eq!(error, "@mode requires a value: check|run");
    }

    #[test]
    fn rejects_empty_required_value() {
        let error =
            Directives::parse("// @mode:   \nfn main() {}\n").expect_err("empty value should fail");

        assert_eq!(error, "@mode requires a value: check|run");
    }

    #[test]
    fn rejects_value_for_flag() {
        let error = Directives::parse("// @helper: nope\nfn main() {}\n")
            .expect_err("flag value should fail");

        assert_eq!(error, "@helper does not take a value");
    }

    #[test]
    fn rejects_duplicate_once_directive() {
        let error = Directives::parse("// @mode: run\n// @mode: check\nfn main() {}\n")
            .expect_err("duplicate directive should fail");

        assert_eq!(error, "duplicate @mode directive");
    }

    #[test]
    fn allows_repeatable_directive() {
        let directives = directives(
            "// @contains: first\n\
             // @contains: second\n\
             fn main() {}\n",
        );

        assert_eq!(
            directives.assertions.selected.contains,
            vec!["first", "second"]
        );
    }

    #[test]
    fn parses_cli_options_in_order() {
        let directives = directives(
            "// @lint: deprecated=allow\n\
             // @feature: gc\n\
             // @cfg: debug\n\
             fn main() {}\n",
        );

        let mut args = vec![];
        directives.cli_options.append_args(&mut args);
        assert_eq!(
            args,
            vec![
                CliFlag::Lint.cli_flag(),
                "deprecated=allow",
                CliFlag::Feature.cli_flag(),
                "gc",
                CliFlag::Cfg.cli_flag(),
                "debug",
            ]
        );
    }

    #[test]
    fn rejects_unclosed_match_block() {
        let error = Directives::parse("// @match-begin\n// A\n// B\n")
            .expect_err("unclosed match block should fail");

        assert_eq!(error, "unclosed @match-begin block");
    }

    #[test]
    fn rejects_non_comment_line_inside_match_block() {
        let error = Directives::parse("// @match-begin\n// A\nfn main() {}\n")
            .expect_err("non-comment inside match block should fail");

        assert_eq!(error, "non-comment line inside @match-begin block");
    }

    #[test]
    fn rejects_match_end_without_begin() {
        let error = Directives::parse("// @match-end\nfn main() {}\n")
            .expect_err("match end without begin should fail");

        assert_eq!(error, "@match-end without @match-begin");
    }

    #[test]
    fn rejects_nested_match_block() {
        let error = Directives::parse("// @match-begin\n// @match-begin\n// B\n// @match-end\n")
            .expect_err("nested match block should fail");

        assert_eq!(error, "nested @match-begin directive");
    }

    #[test]
    fn rejects_single_line_match_block() {
        let error = Directives::parse("// @match-begin\n// A\n// @match-end\n")
            .expect_err("single-line match block should fail");

        assert_eq!(error, "@match-begin block requires at least two lines");
    }

    #[test]
    fn requires_mode_for_non_helper() {
        let error = Directives::parse("// @expect: success\nfn main() {}\n")
            .expect_err("missing @mode should fail");

        assert_eq!(error, "missing @mode directive");
    }

    #[test]
    fn requires_expect_for_non_helper() {
        let error = Directives::parse("// @mode: run\nfn main() {}\n")
            .expect_err("missing @expect should fail");

        assert_eq!(error, "missing @expect directive");
    }

    #[test]
    fn helper_rejects_other_directives() {
        let error = Directives::parse("// @helper\n// @mode: run\nfn main() {}\n")
            .expect_err("helper with other directives should fail");

        assert_eq!(error, "@helper cannot be combined with other directives");
    }

    #[test]
    fn skip_allows_expectation_directives() {
        let directives = directives("// @skip: not today\nfn main() {}\n");

        assert_eq!(directives.skip.as_deref(), Some("not today"));
    }

    #[test]
    fn rejects_stdin_in_check_mode() {
        let error = Directives::parse(&check_fixture("// @stdin: input\nfn main() {}\n"))
            .expect_err("stdin in check mode should fail");

        assert_eq!(error, "@stdin is only valid in @mode: run");
    }

    #[test]
    fn rejects_stdin_empty_line_in_check_mode() {
        let error = Directives::parse(&check_fixture("// @stdin-empty-line\nfn main() {}\n"))
            .expect_err("stdin-empty-line in check mode should fail");

        assert_eq!(error, "@stdin is only valid in @mode: run");
    }

    #[test]
    fn rejects_warn_contains_without_success_expectation() {
        let error = Directives::parse(
            "// @mode: run\n// @expect: error\n// @warn-contains: warning\nfn main() {}\n",
        )
        .expect_err("warn expectation without success should fail");

        assert_eq!(error, "@warn-contains requires @expect: success");
    }

    #[test]
    fn rejects_match_and_contains_conflict() {
        let error = Directives::parse(&fixture(
            "// @match: exact\n// @contains: partial\nfn main() {}\n",
        ))
        .expect_err("exact and contains output checks should conflict");

        assert_eq!(error, "@contains conflicts with exact output matching");
    }

    #[test]
    fn rejects_match_block_and_contains_conflict() {
        let error = Directives::parse(&fixture(
            "// @match-begin\n// A\n// B\n// @match-end\n// @contains: A\nfn main() {}\n",
        ))
        .expect_err("match block and contains output checks should conflict");

        assert_eq!(error, "@contains conflicts with exact output matching");
    }

    #[test]
    fn rejects_match_and_match_block_conflict() {
        let error = Directives::parse(&fixture(
            "// @match: exact\n// @match-begin\n// A\n// B\n// @match-end\nfn main() {}\n",
        ))
        .expect_err("single-line and block exact output should conflict");

        assert_eq!(error, "@match conflicts with @match-begin");
    }

    #[test]
    fn rejects_stderr_match_and_contains_conflict() {
        let error = Directives::parse(&fixture(
            "// @stderr-match: exact\n// @stderr-contains: partial\nfn main() {}\n",
        ))
        .expect_err("exact and contains stderr checks should conflict");

        assert_eq!(error, "@stderr-contains conflicts with @stderr-match");
    }
}
