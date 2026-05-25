mod args;
mod directives;
mod model;
mod report;
mod run_test;

use std::{
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use args::{BackendArg, usage};
use model::{FailurePhase, Mode, RunTestResult, TestResult};
use rayon::{
    ThreadPoolBuilder,
    iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator},
};
use report::Summary;
use run_test::{Cli, is_batch_eligible, plan_test_file, run_binary_case, run_test_file};

const EXT: &str = "anv";

fn main() {
    let args = args::RunnerArgs::new().unwrap_or_else(|e| {
        eprintln!("Error: {e}");
        eprintln!();
        eprintln!("{}", usage());
        std::process::exit(1);
    });

    let cli =
        Cli::build(args.release, !args.report_json, args.new_frontend()).unwrap_or_else(|e| {
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

    let work = expand_backend_work(&files, args.backend);

    if !args.report_json {
        report::print_start(work.len());
    }

    let runtime_timeout = Duration::from_millis(args.timeout_ms);
    let compile_timeout = Duration::from_millis(args.compile_timeout_ms);
    let run_work = || {
        if args.new_frontend() && args.backend == BackendArg::Rust {
            run_new_frontend_rust_work(&work, runtime_timeout, compile_timeout, &cli)
        } else {
            work.par_iter()
                .map(|(file, backend)| {
                    let result =
                        run_test_file(file, runtime_timeout, compile_timeout, *backend, &cli)
                            .unwrap_or_else(|e| {
                                runner_error(FailurePhase::Compile, Mode::Check, None, e)
                            });
                    (file.clone(), *backend, result)
                })
                .collect::<Vec<_>>()
        }
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

fn run_new_frontend_rust_work(
    work: &[(PathBuf, Option<&'static str>)],
    runtime_timeout: Duration,
    compile_timeout: Duration,
    cli: &Cli,
) -> Vec<(PathBuf, Option<&'static str>, RunTestResult)> {
    let mut batch_plans = vec![];
    let mut other = vec![];

    for (file, backend) in work {
        match plan_test_file(file, runtime_timeout, compile_timeout, *backend, true) {
            Ok(plan) if is_batch_eligible(&plan) => {
                batch_plans.push((file.clone(), *backend, plan));
            }
            _ => other.push((file.clone(), *backend)),
        }
    }

    let mut results = other
        .par_iter()
        .map(|(file, backend)| {
            let result = run_test_file(file, runtime_timeout, compile_timeout, *backend, cli)
                .unwrap_or_else(|e| runner_error(FailurePhase::Compile, Mode::Check, None, e));
            (file.clone(), *backend, result)
        })
        .collect::<Vec<_>>();

    if batch_plans.is_empty() {
        return results;
    }

    let batch_frontend = batch_frontend_config(cli.release());
    let batch_input = anvyx_project::rust::CleanRustBatchInput {
        cases: batch_plans
            .iter()
            .map(|(_, _, plan)| {
                let run_test::TestPlan::Run { case, .. } = plan else {
                    unreachable!("batch plans are runnable")
                };
                anvyx_project::rust::CleanRustBatchCase {
                    file: case.file.clone(),
                    frontend: batch_frontend.clone(),
                }
            })
            .collect(),
        cargo_profile: anvyx_project::rust::RustCargoProfile::from_release(cli.release()),
        cache_root: None,
        timeout: Some(compile_timeout),
    };

    match anvyx_project::rust::build_clean_rust_batch(batch_input) {
        Ok(output) => {
            let binaries = output
                .binaries
                .into_iter()
                .map(|binary| (binary.file, binary.binary))
                .collect::<std::collections::HashMap<_, _>>();
            let batch_results = batch_plans
                .into_par_iter()
                .map(|(file, backend, plan)| {
                    let result = binaries
                        .get(&file)
                        .ok_or_else(|| "batch build did not return binary".to_string())
                        .and_then(|binary| run_binary_case(plan, binary))
                        .unwrap_or_else(|e| {
                            runner_error(FailurePhase::Runtime, Mode::Run, backend, e)
                        });
                    (file, backend, result)
                })
                .collect::<Vec<_>>();
            results.extend(batch_results);
        }
        Err(error) => {
            let mut fallback_failed = false;
            let mut batch_results = vec![];
            for (file, backend, _) in batch_plans {
                let result = run_test_file(&file, runtime_timeout, compile_timeout, backend, cli)
                    .unwrap_or_else(|e| runner_error(FailurePhase::Compile, Mode::Check, None, e));
                if !matches!(
                    result.result,
                    TestResult::Pass | TestResult::Skip { .. } | TestResult::Helper
                ) {
                    fallback_failed = true;
                }
                batch_results.push((file, backend, result));
            }
            if fallback_failed {
                results.extend(batch_results);
            } else {
                results.extend(
                    batch_results
                        .into_iter()
                        .map(|(file, backend, mut result)| {
                            result.result = fail_result(
                                FailurePhase::Compile,
                                format!("Batch build failed but per-case fallback passed: {error}"),
                            );
                            (file, backend, result)
                        }),
                );
            }
        }
    }
    results
}

fn runner_error(
    phase: FailurePhase,
    mode: Mode,
    backend: Option<&'static str>,
    error: impl std::fmt::Display,
) -> RunTestResult {
    RunTestResult {
        result: fail_result(phase, format!("Test runner error: {error}")),
        mode,
        backend,
        duration: Duration::ZERO,
    }
}

fn fail_result(phase: FailurePhase, message: String) -> TestResult {
    TestResult::Fail { phase, message }
}

fn batch_frontend_config(release: bool) -> anvyx_lang2::FrontendConfig {
    let mut config = anvyx_lang2::FrontendConfig::default();
    if release {
        config.context.profile = anvyx_lang2::Profile::Release;
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

    use super::{BackendArg, batch_frontend_config, expand_backend_work};

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

    #[test]
    fn batch_frontend_uses_release_profile() {
        assert_eq!(
            batch_frontend_config(false).context.profile,
            anvyx_lang2::Profile::Debug
        );
        assert_eq!(
            batch_frontend_config(true).context.profile,
            anvyx_lang2::Profile::Release
        );
    }
}
