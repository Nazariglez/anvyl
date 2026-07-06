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
