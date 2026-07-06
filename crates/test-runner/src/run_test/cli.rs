use std::{
    io::{ErrorKind, Write},
    path::PathBuf,
    time::Duration,
};

use wait_timeout::ChildExt;

use super::{CliCase, ProcessOutcome, classifier::phase_from_stderr_marker, read_child_output};
use crate::model::{FailurePhase, Mode};

const ORANGE: &str = "\x1b[93m";
const RESET: &str = "\x1b[0m";

pub(crate) struct Cli {
    exe: String,
    release: bool,
}

impl Cli {
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
        Ok(Self { exe, release })
    }

    pub(crate) fn release(&self) -> bool {
        self.release
    }

    pub(crate) fn run(&self, case: &CliCase) -> Result<ProcessOutcome, String> {
        spawn(&self.exe, case, self.release)
    }
}

fn spawn(cmd: &str, case: &CliCase, release: bool) -> Result<ProcessOutcome, String> {
    use std::process::Stdio;

    let mut command = std::process::Command::new(cmd);
    command.args(child_args(case, release));

    let mut child = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| e.to_string())?;

    if let Err(err) = write_child_stdin(child.stdin.take(), &case.stdin) {
        let _ = child.kill();
        let _ = child.wait();
        return Err(err);
    }

    let timeout = process_timeout_for_mode(case.mode, case.runtime_timeout, case.compile_timeout);
    let res = child.wait_timeout(timeout).map_err(|e| e.to_string())?;
    if let Some(status) = res {
        let (stdout, stderr) = read_child_output(&mut child);
        return Ok(ProcessOutcome::Completed {
            exit_code: status.code(),
            stdout,
            stderr,
        });
    }

    let _ = child.kill();
    let _ = child.wait();
    let (_, stderr) = read_child_output(&mut child);
    Ok(ProcessOutcome::Timeout {
        phase: timeout_phase_for_output(case.mode, &stderr),
    })
}

fn child_args(case: &CliCase, release: bool) -> Vec<String> {
    let mut args = vec![case.mode.as_str().to_string()];
    if case.mode == Mode::Run && release {
        args.push("--release".to_string());
    }
    case.cli_options.append_args(&mut args);
    args.push(case.file.display().to_string());
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
        path::PathBuf,
        time::Duration,
    };

    use super::{
        child_args, process_timeout_for_mode, timeout_phase_for_output, write_child_stdin,
    };
    use crate::{
        directives::CliOptions,
        model::{FailurePhase, Mode},
        run_test::CliCase,
    };

    fn case(mode: Mode) -> CliCase {
        CliCase {
            file: PathBuf::from(match mode {
                Mode::Check => "tests/syntax/basic_ok.anv",
                Mode::Run => "tests/run/basic_ok.anv",
            }),
            mode,
            runtime_timeout: Duration::from_millis(2),
            compile_timeout: Duration::from_millis(300),
            cli_options: CliOptions::default(),
            stdin: String::new(),
        }
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
    fn run_forwards_release() {
        assert_eq!(
            child_args(&case(Mode::Run), true),
            vec!["run", "--release", "tests/run/basic_ok.anv"]
        );
    }

    #[test]
    fn writes_stdin() {
        let mut written = Vec::new();

        write_child_stdin(Some(&mut written), "first\n\nthird\n").unwrap();

        assert_eq!(written, b"first\n\nthird\n");
    }

    #[test]
    fn broken_pipe_is_ok() {
        assert!(write_child_stdin(Some(BrokenPipeWriter), "input\n").is_ok());
    }

    #[test]
    fn timeout_uses_marker() {
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
    fn check_timeout() {
        let timeout = process_timeout_for_mode(
            Mode::Check,
            Duration::from_millis(2),
            Duration::from_millis(300),
        );
        assert_eq!(timeout, Duration::from_millis(300));
    }

    #[test]
    fn run_timeout_includes_compile_budget() {
        let timeout = process_timeout_for_mode(
            Mode::Run,
            Duration::from_millis(2),
            Duration::from_millis(300),
        );
        assert_eq!(timeout, Duration::from_millis(302));
    }
}
