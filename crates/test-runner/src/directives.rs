use crate::{ExpectedResult, run_test::Mode};

#[derive(Debug, Default, Clone)]
pub struct Directives {
    pub mode: Mode,
    pub expect: ExpectedResult,
    pub match_exact: Option<String>,
    pub contains: Vec<String>,
    pub skip: Option<String>,
}

impl Directives {
    pub fn new(src: &str) -> Self {
        let mut directives = Self::default();

        let mut is_match_block = false;
        let mut match_block_lines = vec![];

        for line in src.lines() {
            let trimmed = line.trim();

            // maybe a bit fragile but I don't want to parse the whole file
            // so we stop here on the first line that is not a directive
            if !trimmed.starts_with("// @") && is_match_block {
                break;
            }

            // let's manage the multiline block
            match trimmed {
                "// @match-begin" => {
                    is_match_block = true;
                    continue;
                }
                "// @match-end" => {
                    is_match_block = false;
                    continue;
                }
                _ => {}
            }

            if is_match_block {
                let Some(ln) = trimmed.strip_prefix("// ") else {
                    continue;
                };
                match_block_lines.push(ln);
                continue;
            }

            // now let's manage single line directives
            if let Some(ln) = trimmed.strip_prefix("// @skip:") {
                directives.skip = Some(ln.trim().to_string());
            }
            if let Some(ln) = trimmed.strip_prefix("// @expect:") {
                directives.expect = ExpectedResult::from_str(ln.trim());
            }
            if let Some(ln) = trimmed.strip_prefix("// @match:") {
                directives.match_exact = Some(ln.trim().to_string());
            }
            if let Some(ln) = trimmed.strip_prefix("// @contains:") {
                directives.contains.push(ln.trim().to_string());
            }
            if let Some(ln) = trimmed.strip_prefix("// @mode:") {
                directives.mode = Mode::from_str(ln.trim());
            }
        }

        if !match_block_lines.is_empty() {
            directives.match_exact = Some(match_block_lines.join("\n"));
        }

        directives
    }
}
