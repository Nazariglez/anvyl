use std::path::{Path, PathBuf};

use crate::resolve::{ModuleId, PackageId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SourceId(u32);

impl SourceId {
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceKind {
    Root,
    PackageRoot { package: PackageId },
    Module { module: ModuleId },
    Prelude,
    Virtual,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    id: SourceId,
    kind: SourceKind,
    label: String,
    path: Option<PathBuf>,
    text: String,
    line_index: LineIndex,
}

impl SourceFile {
    pub fn id(&self) -> SourceId {
        self.id
    }

    pub fn kind(&self) -> &SourceKind {
        &self.kind
    }

    pub fn label(&self) -> &str {
        &self.label
    }

    pub fn path(&self) -> Option<&Path> {
        self.path.as_deref()
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn line_index(&self) -> &LineIndex {
        &self.line_index
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SourceTable {
    files: Vec<SourceFile>,
}

impl SourceTable {
    pub fn add(
        &mut self,
        kind: SourceKind,
        label: impl Into<String>,
        path: Option<PathBuf>,
        text: impl Into<String>,
    ) -> SourceId {
        let id = SourceId(u32::try_from(self.files.len()).expect("source table exceeded u32 ids"));
        let text = text.into();
        self.files.push(SourceFile {
            id,
            kind,
            label: label.into(),
            path,
            line_index: LineIndex::new(&text),
            text,
        });
        id
    }

    pub fn get(&self, id: SourceId) -> Option<&SourceFile> {
        self.files.get(id.index()).filter(|file| file.id == id)
    }

    pub fn iter(&self) -> impl Iterator<Item = &SourceFile> {
        self.files.iter()
    }

    pub fn len(&self) -> usize {
        self.files.len()
    }

    pub fn is_empty(&self) -> bool {
        self.files.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    len: usize,
    line_starts: Vec<usize>,
    line_ends: Vec<usize>,
    utf16_lines: Vec<Utf16Line>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Utf16Line {
    byte_len: usize,
    utf16_len: u32,
    boundaries: Vec<Utf16Boundary>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Utf16Boundary {
    byte: usize,
    utf16: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LspPosition {
    pub line: u32,
    pub character: u32,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        let mut line_starts = vec![0];
        let mut line_ends = vec![];
        let mut utf16_lines = vec![];
        let bytes = text.as_bytes();
        let mut start = 0;

        for (index, byte) in bytes.iter().enumerate() {
            if *byte != b'\n' {
                continue;
            }
            let end = if index > start && bytes[index - 1] == b'\r' {
                index - 1
            } else {
                index
            };
            line_ends.push(end);
            utf16_lines.push(Utf16Line::new(&text[start..end]));
            start = index + 1;
            line_starts.push(start);
        }

        line_ends.push(text.len());
        utf16_lines.push(Utf16Line::new(&text[start..]));

        Self {
            len: text.len(),
            line_starts,
            line_ends,
            utf16_lines,
        }
    }

    pub fn byte_to_line_col(&self, byte: usize) -> Option<LineCol> {
        if byte > self.len {
            return None;
        }
        let line = self.line_for_byte(byte)?;
        Some(LineCol {
            line: line as u32,
            column: (byte - self.line_starts[line]) as u32,
        })
    }

    pub fn byte_to_lsp_position(&self, byte: usize) -> Option<LspPosition> {
        if byte > self.len {
            return None;
        }
        let line = self.line_for_byte(byte)?;
        let start = self.line_starts[line];
        let end = self.line_ends[line];
        let byte_column = byte.saturating_sub(start).min(end - start);
        Some(LspPosition {
            line: line as u32,
            character: self.utf16_lines[line].byte_to_utf16(byte_column)?,
        })
    }

    pub fn lsp_position_to_byte(&self, position: LspPosition) -> Option<usize> {
        let line = position.line as usize;
        let line_start = *self.line_starts.get(line)?;
        let column = self
            .utf16_lines
            .get(line)?
            .utf16_to_byte(position.character)?;
        Some(line_start + column)
    }

    fn line_for_byte(&self, byte: usize) -> Option<usize> {
        let index = self.line_starts.partition_point(|start| *start <= byte);
        index.checked_sub(1)
    }
}

impl Utf16Line {
    fn new(text: &str) -> Self {
        let mut boundaries = Vec::new();
        let mut utf16 = 0;
        let mut has_non_ascii = false;

        for (byte, ch) in text.char_indices() {
            boundaries.push(Utf16Boundary { byte, utf16 });
            has_non_ascii |= ch.len_utf8() != ch.len_utf16();
            utf16 += ch.len_utf16() as u32;
        }

        boundaries.push(Utf16Boundary {
            byte: text.len(),
            utf16,
        });
        if !has_non_ascii {
            boundaries.clear();
        }

        Self {
            byte_len: text.len(),
            utf16_len: utf16,
            boundaries,
        }
    }

    fn byte_to_utf16(&self, byte: usize) -> Option<u32> {
        if byte > self.byte_len {
            return None;
        }
        if self.boundaries.is_empty() {
            return Some(byte as u32);
        }
        self.boundaries
            .binary_search_by_key(&byte, |boundary| boundary.byte)
            .ok()
            .map(|index| self.boundaries[index].utf16)
    }

    fn utf16_to_byte(&self, utf16: u32) -> Option<usize> {
        if utf16 > self.utf16_len {
            return None;
        }
        if self.boundaries.is_empty() {
            return Some(utf16 as usize);
        }
        self.boundaries
            .binary_search_by_key(&utf16, |boundary| boundary.utf16)
            .ok()
            .map(|index| self.boundaries[index].byte)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source_table_with(texts: &[&str]) -> SourceTable {
        let mut table = SourceTable::default();
        for (index, text) in texts.iter().enumerate() {
            table.add(SourceKind::Virtual, format!("source{index}"), None, *text);
        }
        table
    }

    #[test]
    fn source_table_allocates_ids() {
        let table = source_table_with(&["one", "two"]);
        let ids = table.iter().map(SourceFile::id).collect::<Vec<_>>();

        assert_eq!(table.len(), 2);
        assert_eq!(ids[0].index(), 0);
        assert_eq!(ids[1].index(), 1);
        assert_eq!(table.get(ids[0]).unwrap().text(), "one");
        assert_eq!(table.get(ids[1]).unwrap().label(), "source1");
    }

    #[test]
    fn ascii_line_columns_are_byte_columns() {
        let index = LineIndex::new("one\ntwo");

        assert_eq!(
            index.byte_to_line_col(0),
            Some(LineCol { line: 0, column: 0 })
        );
        assert_eq!(
            index.byte_to_line_col(2),
            Some(LineCol { line: 0, column: 2 })
        );
        assert_eq!(
            index.byte_to_line_col(4),
            Some(LineCol { line: 1, column: 0 })
        );
        assert_eq!(
            index.byte_to_line_col(7),
            Some(LineCol { line: 1, column: 3 })
        );
        assert_eq!(index.byte_to_line_col(8), None);
    }

    #[test]
    fn multibyte_utf8_keeps_byte_columns() {
        let index = LineIndex::new("aéx");

        assert_eq!(
            index.byte_to_line_col(1),
            Some(LineCol { line: 0, column: 1 })
        );
        assert_eq!(
            index.byte_to_line_col(3),
            Some(LineCol { line: 0, column: 3 })
        );
        assert_eq!(
            index.byte_to_line_col(4),
            Some(LineCol { line: 0, column: 4 })
        );
        assert_eq!(
            index.byte_to_lsp_position(1),
            Some(LspPosition {
                line: 0,
                character: 1
            })
        );
        assert_eq!(
            index.byte_to_lsp_position(3),
            Some(LspPosition {
                line: 0,
                character: 2
            })
        );
        assert_eq!(
            index.byte_to_lsp_position(4),
            Some(LspPosition {
                line: 0,
                character: 3
            })
        );
    }

    #[test]
    fn emoji_counts_as_two_lsp_characters() {
        let index = LineIndex::new("😀x");

        assert_eq!(
            index.byte_to_line_col(4),
            Some(LineCol { line: 0, column: 4 })
        );
        assert_eq!(
            index.byte_to_lsp_position(4),
            Some(LspPosition {
                line: 0,
                character: 2
            })
        );
        assert_eq!(
            index.byte_to_lsp_position(5),
            Some(LspPosition {
                line: 0,
                character: 3
            })
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 1
            }),
            None
        );
    }

    #[test]
    fn eof_empty_span_position_is_valid() {
        let index = LineIndex::new("a\n");

        assert_eq!(
            index.byte_to_line_col(2),
            Some(LineCol { line: 1, column: 0 })
        );
        assert_eq!(
            index.byte_to_lsp_position(2),
            Some(LspPosition {
                line: 1,
                character: 0
            })
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 1,
                character: 0
            }),
            Some(2)
        );
    }

    #[test]
    fn lsp_positions_map_back_to_byte_offsets() {
        let index = LineIndex::new("é😀x");

        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 0
            }),
            Some(0)
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 1
            }),
            Some(2)
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 3
            }),
            Some(6)
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 4
            }),
            Some(7)
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 5
            }),
            None
        );
    }

    #[test]
    fn invalid_lsp_positions_inside_surrogate_pairs_are_rejected() {
        let index = LineIndex::new("😀");

        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 1
            }),
            None
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 0,
                character: 2
            }),
            Some(4)
        );
    }

    #[test]
    fn crlf_starts_next_line_after_newline() {
        let index = LineIndex::new("a\r\nb");

        assert_eq!(
            index.byte_to_line_col(0),
            Some(LineCol { line: 0, column: 0 })
        );
        assert_eq!(
            index.byte_to_lsp_position(1),
            Some(LspPosition {
                line: 0,
                character: 1
            })
        );
        assert_eq!(
            index.byte_to_lsp_position(2),
            Some(LspPosition {
                line: 0,
                character: 1
            })
        );
        assert_eq!(
            index.byte_to_line_col(3),
            Some(LineCol { line: 1, column: 0 })
        );
        assert_eq!(
            index.lsp_position_to_byte(LspPosition {
                line: 1,
                character: 0
            }),
            Some(3)
        );
    }
}
