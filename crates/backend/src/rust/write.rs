use std::fmt;

#[derive(Default)]
pub(super) struct RustWriter {
    out: String,
    indent: usize,
}

impl RustWriter {
    pub(super) fn line(&mut self, text: impl fmt::Display) {
        self.write_indent();
        self.write_fmt(format_args!("{text}"));
        self.out.push('\n');
    }

    pub(super) fn blank(&mut self) {
        self.out.push('\n');
    }

    pub(super) fn indented(&mut self, f: impl FnOnce(&mut Self)) {
        self.indent += 1;
        f(self);
        self.indent -= 1;
    }

    pub(super) fn block(&mut self, header: impl fmt::Display, f: impl FnOnce(&mut Self)) {
        self.line(format_args!("{header} {{"));
        self.indented(f);
        self.line("}");
    }

    pub(super) fn push_indent(&mut self) {
        self.indent += 1;
    }

    pub(super) fn pop_indent(&mut self) {
        debug_assert!(self.indent > 0);
        self.indent -= 1;
    }

    pub(super) fn finish(self) -> String {
        self.out
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.out.push_str("    ");
        }
    }

    fn write_fmt(&mut self, args: fmt::Arguments<'_>) {
        use fmt::Write as _;
        self.out.write_fmt(args).expect("write to String failed");
    }
}

#[cfg(test)]
mod tests {
    use super::RustWriter;

    #[test]
    fn writes_single_line() {
        let mut w = RustWriter::default();

        w.line(format_args!("let x = {};", 1));

        assert_eq!(w.finish(), "let x = 1;\n");
    }

    #[test]
    fn indents_nested_lines() {
        let mut w = RustWriter::default();

        w.line("root");
        w.indented(|w| {
            w.line("child");
            w.indented(|w| w.line("leaf"));
            w.line("child2");
        });
        w.line("root2");

        assert_eq!(
            w.finish(),
            "root\n    child\n        leaf\n    child2\nroot2\n"
        );
    }

    #[test]
    fn writes_blocks() {
        let mut w = RustWriter::default();

        w.block("fn main()", |w| {
            w.line("let x = 1;");
        });

        assert_eq!(w.finish(), "fn main() {\n    let x = 1;\n}\n");
    }

    #[test]
    fn blank_lines_have_no_whitespace() {
        let mut w = RustWriter::default();

        w.line("a");
        w.indented(RustWriter::blank);
        w.line("b");

        assert_eq!(w.finish(), "a\n\nb\n");
    }

    #[test]
    fn restores_indentation_after_nested_closures() {
        let mut w = RustWriter::default();

        w.indented(|w| {
            w.line("a");
            w.indented(|w| w.line("b"));
            w.line("c");
        });
        w.line("d");

        assert_eq!(w.finish(), "    a\n        b\n    c\nd\n");
    }
}
