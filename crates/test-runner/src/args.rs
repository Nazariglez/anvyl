use std::path::PathBuf;

#[derive(Debug)]
pub struct RunnerArgs {
    pub root: PathBuf,
    pub timeout_ms: u64,
    pub quiet: bool,
    pub release: bool,
    pub file: Option<PathBuf>,
}

impl RunnerArgs {
    pub fn new() -> Result<Self, String> {
        let args = std::env::args().collect::<Vec<String>>();
        let (root, file) = parse_root_file(&args)?;
        let quiet = parse_quiet(&args);
        let release = parse_release(&args);
        let timeout_ms = parse_timeout(&args);
        Ok(Self {
            root,
            timeout_ms,
            quiet,
            release,
            file,
        })
    }
}

fn parse_quiet(args: &[String]) -> bool {
    args.iter().find(|arg| arg.as_str() == "--quiet").is_some()
}

fn parse_release(args: &[String]) -> bool {
    args.iter()
        .find(|arg| arg.as_str() == "--release")
        .is_some()
}

fn parse_timeout(args: &[String]) -> u64 {
    args.iter()
        .position(|arg| arg.as_str() == "--timeout")
        .and_then(|i| args.get(i + 1))
        .map(|arg| arg.parse::<u64>().unwrap())
        .unwrap_or(2000)
}

fn parse_root_file(args: &[String]) -> Result<(PathBuf, Option<PathBuf>), String> {
    if args.len() == 1 || args[1].starts_with("--") {
        return Err("Provide a directory or a file as first argument".to_string());
    }

    let dir = std::path::Path::new(&args[1]);
    if dir.is_dir() {
        return Ok((dir.to_path_buf(), None));
    }

    if dir.is_file() {
        return Ok((dir.parent().unwrap().to_path_buf(), Some(dir.to_path_buf())));
    }

    Err("Provide a directory or a file as first argument".to_string())
}
