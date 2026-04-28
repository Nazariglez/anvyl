use std::{
    io::{ErrorKind, Read, Write},
    path::{Path, PathBuf},
    time::Duration,
};

use wait_timeout::ChildExt;

use super::{
    classifier::phase_from_stderr_marker,
    driver::{DriverCapabilities, ProcessOutcome, TestCase, TestDriver},
};
use crate::{
    directives::DriverOptions,
    model::{FailurePhase, Mode},
};

const ORANGE: &str = "\x1b[93m";
const RESET: &str = "\x1b[0m";

pub(crate) struct CliDriver {
    exe: String,
}

impl CliDriver {
    pub(crate) fn build(release: bool, announce: bool) -> Result<Self, String> {
        if announce {
            println!(
                "{ORANGE}Compiling anvyx{}{RESET}",
                if release { " (release)..." } else { "..." }
            );
        }
        let mut command = std::process::Command::new("cargo");
        command.arg("build").arg("--package").arg("anvyx");
        if release {
            command.arg("--release");
        }
        let output = command.output().map_err(|e| e.to_string())?;
        if !output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(format!("Build failed\n{stdout}{stderr}"));
        }

        let profile = if release { "release" } else { "debug" };
        let exe_name = if cfg!(target_os = "windows") {
            "anvyx.exe"
        } else {
            "anvyx"
        };
        let target_root =
            std::env::var("CARGO_TARGET_DIR").unwrap_or_else(|_| "target".to_string());
        let exe = PathBuf::from(target_root)
            .join(profile)
            .join(exe_name)
            .display()
            .to_string();
        Ok(Self { exe })
    }
}

impl TestDriver for CliDriver {
    fn run(&self, case: &TestCase<'_>) -> Result<ProcessOutcome, String> {
        spawn_test_process(&self.exe, case)
    }

    fn capabilities(&self) -> DriverCapabilities {
        DriverCapabilities {
            supports_run: true,
            supports_text_diagnostics: true,
        }
    }
}

fn spawn_test_process(cmd: &str, case: &TestCase<'_>) -> Result<ProcessOutcome, String> {
    use std::process::Stdio;

    let mut command = std::process::Command::new(cmd);
    command.args(test_process_args(
        case.file,
        case.mode,
        case.backend,
        case.driver_options,
    ));

    let mut child = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    if let Err(err) = write_child_stdin(child.stdin.take(), case.stdin_text) {
        let _ = child.kill();
        let _ = child.wait();
        return Err(err);
    }

    let timeout = process_timeout_for_mode(case.mode, case.runtime_timeout, case.compile_timeout);
    let res = child.wait_timeout(timeout).map_err(|e| e.to_string())?;
    if let Some(status) = res {
        let (stdout, stderr) = read_child_output(&mut child);
        return Ok(process_outcome_from_exit_code(
            status.code(),
            stdout,
            stderr,
        ));
    }

    let _ = child.kill();
    let _ = child.wait();
    let (_, stderr) = read_child_output(&mut child);
    Ok(ProcessOutcome::Timeout {
        phase: timeout_phase_for_output(case.mode, &stderr),
    })
}

fn process_outcome_from_exit_code(
    code: Option<i32>,
    stdout: String,
    stderr: String,
) -> ProcessOutcome {
    ProcessOutcome::Completed {
        exit_code: code,
        stdout,
        stderr,
    }
}

fn read_child_output(child: &mut std::process::Child) -> (String, String) {
    let mut stdout = String::new();
    let mut stderr = String::new();

    if let Some(mut out) = child.stdout.take() {
        let _ = out.read_to_string(&mut stdout);
    }
    if let Some(mut err) = child.stderr.take() {
        let _ = err.read_to_string(&mut stderr);
    }

    (stdout, stderr)
}

fn test_process_args(
    file: &Path,
    mode: Mode,
    backend: Option<&str>,
    driver_options: &DriverOptions,
) -> Vec<String> {
    let mut args = vec![mode.as_str().to_string()];

    if let Some(backend) = backend {
        args.extend(["--backend".to_string(), backend.to_string()]);
    }
    driver_options.append_cli_args(&mut args);

    args.push(file.display().to_string());
    args
}

fn write_child_stdin<W: Write>(stdin: Option<W>, stdin_text: &str) -> Result<(), String> {
    let Some(mut stdin) = stdin else {
        return Ok(());
    };

    match stdin.write_all(stdin_text.as_bytes()) {
        Ok(()) => Ok(()),
        Err(err) if err.kind() == ErrorKind::BrokenPipe => Ok(()),
        Err(err) => Err(err.to_string()),
    }
}

fn process_timeout_for_mode(
    mode: Mode,
    runtime_timeout: Duration,
    compile_timeout: Duration,
) -> Duration {
    match mode {
        Mode::Check => compile_timeout,
        Mode::Run => compile_timeout.saturating_add(runtime_timeout),
    }
}

fn timeout_phase_for_mode(mode: Mode) -> FailurePhase {
    match mode {
        Mode::Check => FailurePhase::Compile,
        Mode::Run => FailurePhase::Runtime,
    }
}

fn timeout_phase_for_output(mode: Mode, stderr: &str) -> FailurePhase {
    phase_from_stderr_marker(stderr).unwrap_or_else(|| timeout_phase_for_mode(mode))
}

#[cfg(test)]
mod tests {
    use std::{
        io::{ErrorKind, Write},
        path::Path,
        time::Duration,
    };

    use super::{
        process_outcome_from_exit_code, process_timeout_for_mode, test_process_args,
        timeout_phase_for_output, write_child_stdin,
    };
    use crate::{
        directives::{DriverFlag, DriverOptions},
        model::{FailurePhase, Mode},
        run_test::driver::ProcessOutcome,
    };

    fn has_arg_pair(args: &[String], flag: &str, value: &str) -> bool {
        args.windows(2)
            .any(|pair| pair[0] == flag && pair[1] == value)
    }

    struct BrokenPipeWriter;

    impl Write for BrokenPipeWriter {
        fn write(&mut self, _buf: &[u8]) -> std::io::Result<usize> {
            Err(std::io::Error::new(ErrorKind::BrokenPipe, "closed"))
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    #[test]
    fn child_cli_args_do_not_include_unsupported_old_flags() {
        let mut driver_options = DriverOptions::default();
        driver_options.push(DriverFlag::Lint, "unused".to_string());
        driver_options.push(DriverFlag::Feature, "gc".to_string());
        driver_options.push(DriverFlag::Cfg, "debug".to_string());
        let args = test_process_args(
            Path::new("tests/run/stdin_ok.anv"),
            Mode::Run,
            Some("vm"),
            &driver_options,
        );

        assert_eq!(args[0], "run");
        assert!(has_arg_pair(&args, "--backend", "vm"));
        assert!(has_arg_pair(&args, "--lint", "unused"));
        assert!(has_arg_pair(&args, "--feature", "gc"));
        assert!(has_arg_pair(&args, "--cfg", "debug"));
        assert!(!args.iter().any(|arg| matches!(
            arg.as_str(),
            "--quiet"
                | "--timeout"
                | "--compile-timeout"
                | "--jobs"
                | "--report-json"
                | "--driver"
                | "--diagnostics"
                | "--"
        )));
    }

    #[test]
    fn check_mode_child_cli_args_have_no_backend_when_effective_backend_is_none() {
        let args = test_process_args(
            Path::new("tests/syntax/basic_ok.anv"),
            Mode::Check,
            None,
            &DriverOptions::default(),
        );

        assert_eq!(args, vec!["check", "tests/syntax/basic_ok.anv"]);
    }

    #[test]
    fn writes_fixture_stdin_to_child_stdin() {
        let mut written = Vec::new();

        write_child_stdin(Some(&mut written), "first\n\nthird\n").unwrap();

        assert_eq!(written, b"first\n\nthird\n");
    }

    #[test]
    fn empty_fixture_stdin_writes_empty_payload() {
        let mut written = Vec::new();

        write_child_stdin(Some(&mut written), "").unwrap();

        assert!(written.is_empty());
    }

    #[test]
    fn broken_pipe_while_writing_stdin_is_not_a_runner_error() {
        assert!(write_child_stdin(Some(BrokenPipeWriter), "input\n").is_ok());
    }

    #[test]
    fn process_exit_preserves_raw_code_and_streams() {
        let outcome = process_outcome_from_exit_code(Some(7), "out".to_string(), "err".to_string());

        assert!(matches!(
            outcome,
            ProcessOutcome::Completed {
                exit_code: Some(7),
                stdout,
                stderr,
            } if stdout == "out" && stderr == "err"
        ));
    }

    #[test]
    fn runner_timeout_uses_marker_then_timeout_mode_fallback() {
        assert_eq!(
            timeout_phase_for_output(Mode::Run, "Compile error: stuck\n"),
            FailurePhase::Compile,
        );
        assert_eq!(
            timeout_phase_for_output(Mode::Run, ""),
            FailurePhase::Runtime,
        );
    }

    #[test]
    fn check_mode_uses_compile_process_timeout() {
        let timeout = process_timeout_for_mode(
            Mode::Check,
            Duration::from_millis(2),
            Duration::from_millis(300),
        );

        assert_eq!(timeout, Duration::from_millis(300));
    }

    #[test]
    fn run_mode_uses_compile_plus_runtime_process_timeout() {
        let timeout = process_timeout_for_mode(
            Mode::Run,
            Duration::from_millis(2),
            Duration::from_millis(300),
        );

        assert_eq!(timeout, Duration::from_millis(302));
    }
}
