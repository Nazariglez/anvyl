use console::style;

const VERB_WIDTH: usize = 12;

pub fn status(verb: &str, message: &str) {
    let padded = format!("{verb:>VERB_WIDTH$}");
    eprintln!("{} {}", style(padded).green().bold(), message);
}

pub fn error(message: &str) {
    let padded = format!("{:>width$}", "error:", width = VERB_WIDTH);
    eprintln!("{} {}", style(padded).red().bold(), message);
}
