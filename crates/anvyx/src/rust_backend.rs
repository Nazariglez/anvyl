use anvyx_lang::AirBuildError;
use anvyx_project::rust::{BuildInput, BuildOutput, Error, Event, RunInput, RunOutput};

use crate::{check, progress};

pub(crate) fn run(input: RunInput) -> Result<RunOutput, String> {
    anvyx_project::rust::run_with_events(input, handle_event).map_err(render_error)
}

pub(crate) fn build(input: BuildInput) -> Result<BuildOutput, String> {
    anvyx_project::rust::build_with_events(input, handle_event).map_err(render_error)
}

fn render_error(error: Error) -> String {
    match error {
        Error::Air(AirBuildError::Diagnostic(output)) => {
            check::emit_text_diagnostic_report(&output.report);
            output.summary().to_string()
        }
        Error::TargetDiagnostics(output) => {
            check::emit_text_diagnostic_report(&output.report);
            output.gaps.summary()
        }
        error => error.to_string(),
    }
}

fn handle_event(event: &Event<'_>) {
    match event {
        Event::Checking { file } => {
            progress::status("Checking", &format!("{}...", file.display()));
        }
        Event::Checked { report } => check::emit_text_diagnostic_report(report),
        Event::GeneratingRust => progress::status("Generating", "Rust..."),
        Event::CompilingRust => progress::status("Compiling", "generated Rust..."),
        Event::Running { file } => {
            progress::status("Running", &format!("{}...", file.display()));
        }
    }
}
