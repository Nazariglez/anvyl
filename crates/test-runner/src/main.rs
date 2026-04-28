mod args;
mod directives;
mod model;
mod report;
mod run_test;

use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use args::{BackendArg, DriverArg, usage};
use model::{FailurePhase, Mode, RunTestResult, TestResult};
use rayon::{
    ThreadPoolBuilder,
    iter::{IntoParallelRefIterator, ParallelIterator},
};
use report::Summary;
use run_test::{CliDriver, FrontendDriver, TestDriver, run_test_file};

const EXT: &str = "anv";

fn main() {
    let args = args::RunnerArgs::new().unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        eprintln!();
        eprintln!("{}", usage());
        std::process::exit(1);
    });

    let driver: Box<dyn TestDriver> = match args.driver {
        DriverArg::Cli => Box::new(CliDriver::build(args.release, !args.report_json).unwrap()),
        DriverArg::Frontend => Box::new(FrontendDriver),
    };

    let start_time = Instant::now();
    let mut files: Vec<_> = args
        .paths
        .iter()
        .flat_map(|path| {
            if path.is_dir() {
                list_all_anv_files(path)
            } else {
                vec![path.clone()]
            }
        })
        .collect();
    files.sort();

    let work = expand_backend_work(&files, args.backend);

    if !args.report_json {
        report::print_start(work.len());
    }

    let runtime_timeout = Duration::from_millis(args.timeout_ms);
    let compile_timeout = Duration::from_millis(args.compile_timeout_ms);
    let run_work = || {
        work.par_iter()
            .map(|(file, backend)| {
                let result = run_test_file(
                    file,
                    runtime_timeout,
                    compile_timeout,
                    *backend,
                    driver.as_ref(),
                )
                .unwrap_or_else(|e| RunTestResult {
                    result: TestResult::Fail {
                        phase: FailurePhase::Compile,
                        message: format!("Test runner error: {e}"),
                    },
                    mode: Mode::Check,
                    backend: None,
                    duration: Duration::ZERO,
                });
                (file.clone(), *backend, result)
            })
            .collect::<Vec<_>>()
    };
    let results = if let Some(jobs) = args.jobs {
        ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build()
            .unwrap_or_else(|e| {
                eprintln!("Error: failed to build test-runner thread pool: {e}");
                std::process::exit(1);
            })
            .install(run_work)
    } else {
        run_work()
    };

    let mut summary = Summary::default();
    for (file, scheduled_backend, result) in results {
        summary.add(
            file,
            scheduled_backend,
            result,
            args.quiet || args.report_json,
        );
    }

    if args.report_json {
        summary.print_json(&args, start_time).unwrap_or_else(|e| {
            eprintln!("Error: failed to serialize JSON report: {e}");
            std::process::exit(1);
        });
    } else {
        summary.print_summary(start_time);
        println!();
    }
}

fn list_all_anv_files(root: &PathBuf) -> Vec<PathBuf> {
    walkdir::WalkDir::new(root)
        .into_iter()
        .filter_map(Result::ok)
        .filter(|entry| {
            entry.file_type().is_file()
                && entry.path().extension().and_then(|s| s.to_str()) == Some(EXT)
        })
        .map(|entry| entry.path().to_path_buf())
        .collect()
}

fn expand_backend_work(
    files: &[PathBuf],
    backend: BackendArg,
) -> Vec<(PathBuf, Option<&'static str>)> {
    files
        .iter()
        .flat_map(|file| {
            backend
                .expand()
                .iter()
                .map(move |backend| (file.clone(), Some(*backend)))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{BackendArg, expand_backend_work};

    #[test]
    fn both_backend_work_uses_vm_then_rust_per_file() {
        let files = vec![PathBuf::from("a.anv"), PathBuf::from("b.anv")];

        let work = expand_backend_work(&files, BackendArg::Both);

        assert_eq!(
            work,
            vec![
                (PathBuf::from("a.anv"), Some("vm")),
                (PathBuf::from("a.anv"), Some("rust")),
                (PathBuf::from("b.anv"), Some("vm")),
                (PathBuf::from("b.anv"), Some("rust")),
            ]
        );
    }
}
