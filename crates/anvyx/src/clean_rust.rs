use anvyx_lang2::AirBuildError;
use anvyx_project::rust::{
    CleanRustBuildInput, CleanRustBuildOutput, CleanRustError, CleanRustEvent, CleanRustRunInput,
    CleanRustRunOutput,
};

use crate::{check, progress};

pub(crate) fn run(input: CleanRustRunInput) -> Result<CleanRustRunOutput, String> {
    anvyx_project::rust::run_clean_rust_with_events(input, handle_event).map_err(render_error)
}

pub(crate) fn build(input: CleanRustBuildInput) -> Result<CleanRustBuildOutput, String> {
    anvyx_project::rust::build_clean_rust_with_events(input, handle_event).map_err(render_error)
}

fn render_error(error: CleanRustError) -> String {
    match error {
        CleanRustError::Air(AirBuildError::Diagnostic(output)) => {
            check::emit_text_diagnostic_report(&output.report);
            output.summary().to_string()
        }
        error => error.to_string(),
    }
}

fn handle_event(event: CleanRustEvent) {
    match event {
        CleanRustEvent::Checking { file } => {
            progress::status("Checking", &format!("{}...", file.display()));
        }
        CleanRustEvent::Checked { report } => check::emit_text_diagnostic_report(&report),
        CleanRustEvent::GeneratingRust => progress::status("Generating", "Rust..."),
        CleanRustEvent::CompilingRust => progress::status("Compiling", "generated Rust..."),
        CleanRustEvent::Running { file } => {
            progress::status("Running", &format!("{}...", file.display()));
        }
    }
}
