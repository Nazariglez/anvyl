mod args;
mod directives;
mod model;
mod report;
mod run_test;

use std::{
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use anvyx_project::rust as project_rust;
use args::usage;
use model::{FailurePhase, Mode, RunTestResult, TestResult};
use rayon::{
    ThreadPoolBuilder,
    iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator},
};
use report::Summary;
use run_test::{Cli, TestPlan, is_batch_eligible, plan_test_file, run_binary_case, run_test_file};

const EXT: &str = "anv";

fn main() {
    let args = args::RunnerArgs::new().unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        eprintln!();
        eprintln!("{}", usage());
        std::process::exit(1);
    });

    let cli = Cli::build(args.release, !args.report_json).unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        std::process::exit(1);
    });

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

    if !args.report_json {
        report::print_start(files.len());
    }

    let runtime_timeout = Duration::from_millis(args.timeout_ms);
    let compile_timeout = Duration::from_millis(args.compile_timeout_ms);
    let execute = || {
        run_tests(
            &files,
            runtime_timeout,
            compile_timeout,
            &cli,
            !args.report_json,
        )
    };
    let results = if let Some(jobs) = args.jobs {
        ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build()
            .unwrap_or_else(|e| {
                eprintln!("Error: failed to build test-runner thread pool: {e}");
                std::process::exit(1);
            })
            .install(execute)
    } else {
        execute()
    };

    let mut summary = Summary::default();
    for (file, result) in results {
        summary.add(file, result, args.quiet || args.report_json);
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

    if !summary.is_success() {
        std::process::exit(1);
    }
}

fn run_tests(
    files: &[PathBuf],
    runtime_timeout: Duration,
    compile_timeout: Duration,
    cli: &Cli,
    show_status: bool,
) -> Vec<(PathBuf, RunTestResult)> {
    let mut batch_plans = vec![];
    let mut other = vec![];

    for file in files {
        match plan_test_file(file, runtime_timeout, compile_timeout) {
            Ok(plan) if is_batch_eligible(&plan) => batch_plans.push((file.clone(), plan)),
            _ => other.push(file.clone()),
        }
    }

    if batch_plans.is_empty() {
        if show_status {
            report::print_test_run();
        }
        return run_other_work(&other, runtime_timeout, compile_timeout, cli);
    }

    let batch_input = project_rust::BatchInput {
        cases: batch_plans
            .iter()
            .map(|(_, plan)| {
                let TestPlan::Run { case, .. } = plan else {
                    unreachable!("batch plans are runnable")
                };
                project_rust::BatchCase {
                    file: case.file.clone(),
                    frontend: batch_frontend_config(cli.release()),
                }
            })
            .collect(),
        cargo_profile: project_rust::RustCargoProfile::from_release(cli.release()),
        cache_root: None,
        timeout: Some(batch_compile_timeout(compile_timeout, batch_plans.len())),
    };

    if show_status {
        report::print_rust_batch_compile(batch_plans.len());
    }
    let batch = project_rust::build_batch(batch_input);
    if show_status {
        report::print_test_run();
    }

    let mut results = run_other_work(&other, runtime_timeout, compile_timeout, cli);
    match batch {
        Ok(output) => {
            let binaries = output
                .binaries
                .into_iter()
                .map(|binary| (binary.file, binary.binary))
                .collect::<std::collections::HashMap<_, _>>();
            let batch_results = batch_plans
                .into_par_iter()
                .map(|(file, plan)| {
                    let result = binaries
                        .get(&file)
                        .ok_or_else(|| "batch build did not return binary".to_string())
                        .and_then(|binary| run_binary_case(plan, binary))
                        .unwrap_or_else(|e| runner_error(FailurePhase::Runtime, Mode::Run, e));
                    (file, result)
                })
                .collect::<Vec<_>>();
            results.extend(batch_results);
        }
        Err(error) => {
            let batch_results = batch_plans
                .into_iter()
                .map(|(file, _)| {
                    (
                        file,
                        runner_error(FailurePhase::Compile, Mode::Run, error.clone()),
                    )
                })
                .collect::<Vec<_>>();
            results.extend(batch_results);
        }
    }
    results
}

fn batch_compile_timeout(per_case: Duration, cases: usize) -> Duration {
    per_case.saturating_mul(u32::try_from(cases).unwrap_or(u32::MAX))
}

fn run_other_work(
    files: &[PathBuf],
    runtime_timeout: Duration,
    compile_timeout: Duration,
    cli: &Cli,
) -> Vec<(PathBuf, RunTestResult)> {
    files
        .par_iter()
        .map(|file| {
            let result = run_test_file(file, runtime_timeout, compile_timeout, cli)
                .unwrap_or_else(|e| runner_error(FailurePhase::Compile, Mode::Check, e));
            (file.clone(), result)
        })
        .collect()
}

fn runner_error(phase: FailurePhase, mode: Mode, error: impl std::fmt::Display) -> RunTestResult {
    RunTestResult {
        result: TestResult::Fail {
            phase,
            message: format!("Test runner error: {error}"),
        },
        mode,
        duration: Duration::ZERO,
    }
}

fn batch_frontend_config(release: bool) -> anvyx_lang::FrontendConfig {
    let mut config = anvyx_lang::FrontendConfig::default();
    if release {
        config.context.profile = anvyx_lang::Profile::Release;
    }
    config
}

fn list_all_anv_files(root: &Path) -> Vec<PathBuf> {
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

#[cfg(test)]
mod tests {
    use super::batch_frontend_config;

    #[test]
    fn batch_frontend_uses_release_profile() {
        assert_eq!(
            batch_frontend_config(false).context.profile,
            anvyx_lang::Profile::Debug
        );
        assert_eq!(
            batch_frontend_config(true).context.profile,
            anvyx_lang::Profile::Release
        );
    }
}
