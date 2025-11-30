mod args;
mod directives;
mod run_test;

use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use run_test::{ExpectedResult, TestResult, run_test_file};
use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use crate::run_test::{Mode, RunTestResult};

const EXT: &str = "anv";
const GREEN: &str = "\x1b[32m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const CYAN: &str = "\x1b[36m";
const BLUE: &str = "\x1b[34m";
const RESET: &str = "\x1b[0m";
const GREY: &str = "\x1b[90m";

fn main() {
    let args = args::RunnerArgs::new().unwrap();

    let exe = run_test::compile_lang(args.release).unwrap();

    let start_time = Instant::now();
    let files = if let Some(file) = args.file {
        vec![file]
    } else {
        list_all_anv_files(&args.root)
    };

    println!("");
    println!("{CYAN}Running {} tests...{RESET}", files.len());
    println!("");

    let results = files
        .par_iter()
        .filter_map(|file| {
            match run_test_file(&exe, file, Duration::from_millis(args.timeout_ms)) {
                Ok(res) => Some((file.clone(), res)),
                Err(e) => Some((
                    file.clone(),
                    run_test::RunTestResult {
                        result: TestResult::Fail {
                            message: format!("Test runner error: {e}"),
                        },
                        mode: run_test::Mode::Check,
                        duration: Duration::from_secs(0),
                    },
                )),
            }
        })
        .collect::<Vec<_>>();

    let mut summary = Summary::default();
    for (file, result) in results {
        summary.add(file, result, args.quiet);
    }
    summary.print_summary(start_time, files.len());
    println!("");
}

#[derive(Debug, Default)]
struct Summary {
    passed: usize,
    failed: usize,
    skipped: usize,
    timed_out: usize,

    failures: Vec<(PathBuf, String)>,
    timeouts: Vec<PathBuf>,
    skips: Vec<(PathBuf, String)>,
}

impl Summary {
    fn add(&mut self, file: PathBuf, result: RunTestResult, quiet: bool) {
        let RunTestResult {
            result,
            mode,
            duration,
        } = result;

        match result {
            TestResult::Pass => {
                self.passed += 1;
                pass_msg(&file, quiet, mode, duration);
            }
            TestResult::Fail { message } => {
                self.failed += 1;
                fail_msg(&file, quiet, mode, duration);
                self.failures.push((file, message));
            }
            TestResult::Timeout => {
                self.timed_out += 1;
                timeout_msg(&file, quiet, mode, duration);
                self.timeouts.push(file);
            }
            TestResult::Skip { message } => {
                self.skipped += 1;
                skip_msg(&file, quiet, mode, duration);
                self.skips.push((file, message));
            }
        }
    }

    fn print_summary(&self, start_time: Instant, total: usize) {
        println!("");
        println!("{CYAN}Summary: {RESET}");
        println!("");

        if self.skipped > 0 {
            println!("* {}Skipped:{} {}", YELLOW, RESET, self.skipped);
            self.skips.iter().for_each(|(f, m)| {
                println!("{YELLOW}  - {}:{RESET}", f.display());
                tab_print(4, m, false);
            });
            println!("");
        }

        if self.timed_out > 0 {
            eprintln!("* {}Timed out:{} {}", BLUE, RESET, self.timed_out);
            self.timeouts
                .iter()
                .for_each(|f| eprintln!("{BLUE}  - {}{RESET}", f.display()));
            println!("");
        }

        if self.failed > 0 {
            eprintln!("* {}Failed:{} {}", RED, RESET, self.failed);
            self.failures.iter().for_each(|(f, m)| {
                println!("");
                eprintln!("{RED}  - {}:{RESET}", f.display());
                tab_print(4, m, true);
            });
            println!("");
        }

        println!("* {GREEN}Passed:{RESET} {}", self.passed);
        println!("");
        let result = format!(
            "{GREEN}{}{RESET} passed; {RED}{}{RESET} failed; {BLUE}{}{RESET} timed out; {YELLOW}{}{RESET} skipped; finished in: {CYAN}{:.2}s{RESET}",
            self.passed,
            self.failed,
            self.timed_out,
            self.skipped,
            start_time.elapsed().as_secs_f64()
        );
        if self.failed > 0 || self.timed_out > 0 {
            eprintln!("Test Result: {RED}FAILED{RESET}. -- {result}");
        } else {
            eprintln!("Test Result: {GREEN}OK{RESET}. -- {result}");
        }
    }
}

fn pass_msg(file: &PathBuf, quiet: bool, mode: Mode, duration: Duration) {
    if quiet {
        return;
    }
    println!(
        "{GREEN}[PASS]{RESET} {} {GREY}({mode} - {:.3}s){RESET}",
        file.display(),
        duration.as_secs_f32()
    );
}

fn fail_msg(file: &PathBuf, quiet: bool, mode: Mode, duration: Duration) {
    if quiet {
        return;
    }
    eprintln!(
        "{RED}[FAIL]{RESET} {} {GREY}({mode} - {:.3}s){RESET}",
        file.display(),
        duration.as_secs_f32()
    );
}

fn timeout_msg(file: &PathBuf, quiet: bool, mode: Mode, duration: Duration) {
    if quiet {
        return;
    }
    eprintln!(
        "{BLUE}[TIMEOUT]{RESET} {} {GREY}({mode} - {:.3}s){RESET}",
        file.display(),
        duration.as_secs_f32()
    );
}

fn skip_msg(file: &PathBuf, quiet: bool, mode: Mode, duration: Duration) {
    if quiet {
        return;
    }
    println!(
        "{YELLOW}[SKIP]{RESET} {} {GREY}({mode} - {:.3}s){RESET}",
        file.display(),
        duration.as_secs_f32()
    );
}

fn list_all_anv_files(root: &PathBuf) -> Vec<PathBuf> {
    walkdir::WalkDir::new(root)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().is_file()
                && entry.path().extension().and_then(|s| s.to_str()) == Some(EXT)
        })
        .map(|entry| entry.path().to_path_buf())
        .collect()
}

fn tab_print(spaces: usize, message: &str, is_error: bool) {
    let message = message.replace("\\n", "\n");
    for line in message.lines() {
        if is_error {
            eprintln!("{:>spaces$}| {line}", "");
        } else {
            println!("{:>spaces$}| {line}", "");
        }
    }
}
