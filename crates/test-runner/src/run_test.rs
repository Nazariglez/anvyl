use anvyl_lang::run_program;

use crate::directives::Directives;
use std::path::PathBuf;

pub fn run_test_file(file: &PathBuf) -> Result<TestResult, String> {
    let src = std::fs::read_to_string(file).map_err(|e| e.to_string())?;
    let directives = Directives::new(&src);
    if directives.skip.is_some() {
        return Ok(TestResult::Skip {
            message: directives.skip.unwrap(),
        });
    }

    Ok(match run_program(&src) {
        Ok(output) => match directives.expect {
            ExpectedResult::Success => match_output(&output, &directives)?,
            ExpectedResult::Error => TestResult::Fail {
                message: "Expected error but got success".to_string(),
            },
            ExpectedResult::Timeout => TestResult::Fail {
                message: "Expected timeout but got success".to_string(),
            },
        },
        Err(err) => match directives.expect {
            ExpectedResult::Error => match_output(&err, &directives)?,
            ExpectedResult::Success => TestResult::Fail {
                message: format!("Expected success but got error:\n{err}"),
            },
            ExpectedResult::Timeout => TestResult::Fail {
                message: format!("Expected timeout but got error:\n{err}"),
            },
        },
    })
}

fn match_output(output: &str, directives: &Directives) -> Result<TestResult, String> {
    // exact match, multilnne or not
    if let Some(expected) = &directives.match_exact {
        let expected_lines = expected.lines();
        let lns = output.lines();
        let same_lines_num = lns.count() == expected_lines.count();
        if !same_lines_num {
            return Ok(TestResult::Fail {
                message: format!("* Expected:\n{output}\n* Got:\n{}", expected),
            });
        }

        let expected_lines = expected.lines();
        let lns = output.lines();
        let join_iter = lns.zip(expected_lines);
        for (idx, (ln, expected_ln)) in join_iter.enumerate() {
            if ln != expected_ln {
                return Ok(TestResult::Fail {
                    message: format!(
                        "*Line {idx} failed\n* Expected:\n{expected}\n* Got:\n{output}",
                    ),
                });
            }
        }

        return Ok(TestResult::Pass);
    }

    // contains match
    let lns = output.lines().collect::<Vec<_>>();
    for (idx, expected_ln) in directives.contains.iter().enumerate() {
        if let Some(ln) = lns.get(idx) {
            if !ln.contains(expected_ln) {
                return Ok(TestResult::Fail {
                    message: format!(
                        "* Line {idx} failed\n* Expected to contain:\n{expected_ln}\n* Got:\n{ln}",
                    ),
                });
            }
        } else {
            return Ok(TestResult::Fail {
                message: format!("* Line {idx} not found\n* Expected to contain:\n{expected_ln}"),
            });
        }
    }

    Ok(TestResult::Pass)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExpectedResult {
    #[default]
    Success,
    Error,
    Timeout,
}

impl ExpectedResult {
    pub fn from_str(s: &str) -> Self {
        match s {
            "success" => Self::Success,
            "error" => Self::Error,
            "timeout" => Self::Timeout,
            _ => panic!("Invalid expected result: {}", s),
        }
    }
}

impl std::fmt::Display for ExpectedResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub enum TestResult {
    Pass,
    Fail { message: String },
    Timeout,
    Skip { message: String },
}
