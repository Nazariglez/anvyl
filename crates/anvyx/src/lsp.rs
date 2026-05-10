use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    io::{self, BufRead, Write},
    path::{Component, Path, PathBuf},
};

use anvyx_lang2::{
    CheckFileInput, Diagnostic, DiagnosticLabel, DiagnosticReport, DiagnosticSeverity, LabelStyle,
    SourceOverride,
};
use serde::Serialize;
use serde_json::{Value, json};

pub fn cmd() -> Result<(), String> {
    let stdin = io::stdin();
    let stdout = io::stdout();
    run_server(&mut stdin.lock(), &mut stdout.lock())
}

fn run_server(reader: &mut impl BufRead, writer: &mut impl Write) -> Result<(), String> {
    let mut adapter = LspAdapter::default();
    let mut shutdown = false;
    while let Some(message) = read_message(reader)? {
        if handle_message(&mut adapter, writer, &message, &mut shutdown)? {
            break;
        }
    }
    Ok(())
}

fn handle_message(
    adapter: &mut LspAdapter,
    writer: &mut impl Write,
    message: &Value,
    shutdown: &mut bool,
) -> Result<bool, String> {
    let method = message.get("method").and_then(Value::as_str);
    match method {
        Some("initialize") => {
            if let Some(id) = message.get("id") {
                write_message(
                    writer,
                    &json!({
                        "jsonrpc": "2.0",
                        "id": id,
                        "result": {
                            "capabilities": {
                                "textDocumentSync": {
                                    "openClose": true,
                                    "change": 1,
                                    "save": true
                                }
                            }
                        }
                    }),
                )?;
            }
        }
        Some("shutdown") => {
            *shutdown = true;
            if let Some(id) = message.get("id") {
                write_message(
                    writer,
                    &json!({ "jsonrpc": "2.0", "id": id, "result": null }),
                )?;
            }
        }
        Some("exit") => return Ok(*shutdown),
        Some("textDocument/didOpen") => {
            let params = message.params()?;
            let document = params
                .get("textDocument")
                .ok_or_else(|| "didOpen missing textDocument".to_string())?;
            let uri = required_str(document, "uri")?;
            let text = required_str(document, "text")?;
            let version = optional_i32(document, "version")?;
            let publishes = adapter.open_document(uri.to_string(), version, text.to_string())?;
            write_publishes(writer, publishes)?;
        }
        Some("textDocument/didChange") => {
            let params = message.params()?;
            let document = params
                .get("textDocument")
                .ok_or_else(|| "didChange missing textDocument".to_string())?;
            let uri = required_str(document, "uri")?;
            let version = optional_i32(document, "version")?;
            let text = params
                .get("contentChanges")
                .and_then(Value::as_array)
                .and_then(|changes| changes.last())
                .and_then(|change| change.get("text"))
                .and_then(Value::as_str)
                .ok_or_else(|| "didChange missing full-text contentChanges entry".to_string())?;
            let publishes = adapter.change_document(uri, version, text.to_string())?;
            write_publishes(writer, publishes)?;
        }
        Some("textDocument/didSave") => {
            let uri = text_document_uri(message.params()?)?;
            let publishes = adapter.save_document(uri)?;
            write_publishes(writer, publishes)?;
        }
        Some("textDocument/didClose") => {
            let uri = text_document_uri(message.params()?)?;
            let publishes = adapter.close_document(uri);
            write_publishes(writer, publishes)?;
        }
        Some(_) | None => {
            if let Some(id) = message.get("id") {
                write_message(
                    writer,
                    &json!({ "jsonrpc": "2.0", "id": id, "result": null }),
                )?;
            }
        }
    }
    Ok(false)
}

trait MessageParams {
    fn params(&self) -> Result<&Value, String>;
}

impl MessageParams for Value {
    fn params(&self) -> Result<&Value, String> {
        self.get("params")
            .ok_or_else(|| "message missing params".to_string())
    }
}

fn text_document_uri(params: &Value) -> Result<&str, String> {
    let document = params
        .get("textDocument")
        .ok_or_else(|| "message missing textDocument".to_string())?;
    required_str(document, "uri")
}

fn required_str<'a>(object: &'a Value, key: &str) -> Result<&'a str, String> {
    object
        .get(key)
        .and_then(Value::as_str)
        .ok_or_else(|| format!("message missing string field '{key}'"))
}

fn optional_i32(object: &Value, key: &str) -> Result<Option<i32>, String> {
    object
        .get(key)
        .map(|value| {
            value
                .as_i64()
                .and_then(|version| i32::try_from(version).ok())
                .ok_or_else(|| format!("message field '{key}' must be an i32"))
        })
        .transpose()
}

fn write_publishes(
    writer: &mut impl Write,
    publishes: Vec<PublishDiagnostics>,
) -> Result<(), String> {
    for publish in publishes {
        write_message(
            writer,
            &json!({
                "jsonrpc": "2.0",
                "method": "textDocument/publishDiagnostics",
                "params": publish
            }),
        )?;
    }
    Ok(())
}

fn read_message(reader: &mut impl BufRead) -> Result<Option<Value>, String> {
    let mut content_len = None;
    loop {
        let mut line = String::new();
        if reader
            .read_line(&mut line)
            .map_err(|error| error.to_string())?
            == 0
        {
            return Ok(None);
        }
        let header = line.trim_end_matches(['\r', '\n']);
        if header.is_empty() {
            break;
        }
        if let Some(value) = header.strip_prefix("Content-Length:") {
            content_len = Some(
                value
                    .trim()
                    .parse::<usize>()
                    .map_err(|error| format!("invalid Content-Length: {error}"))?,
            );
        }
    }

    let Some(content_len) = content_len else {
        return Err("message missing Content-Length".to_string());
    };
    let mut body = vec![0; content_len];
    reader
        .read_exact(&mut body)
        .map_err(|error| error.to_string())?;
    serde_json::from_slice(&body).map_err(|error| error.to_string())
}

fn write_message(writer: &mut impl Write, message: &Value) -> Result<(), String> {
    let body = serde_json::to_vec(message).map_err(|error| error.to_string())?;
    write!(writer, "Content-Length: {}\r\n\r\n", body.len()).map_err(|error| error.to_string())?;
    writer.write_all(&body).map_err(|error| error.to_string())?;
    writer.flush().map_err(|error| error.to_string())
}

#[derive(Debug, Default)]
pub struct LspAdapter {
    documents: DocumentStore,
    published_uris: HashSet<String>,
}

impl LspAdapter {
    pub fn open_document(
        &mut self,
        uri: impl Into<String>,
        version: Option<i32>,
        text: impl Into<String>,
    ) -> Result<Vec<PublishDiagnostics>, String> {
        let uri = uri.into();
        self.documents.open(uri.clone(), version, text);
        self.check_open_document(&uri)
    }

    pub fn change_document(
        &mut self,
        uri: &str,
        version: Option<i32>,
        text: impl Into<String>,
    ) -> Result<Vec<PublishDiagnostics>, String> {
        self.documents.change(uri, version, text);
        self.check_open_document(uri)
    }

    pub fn save_document(&mut self, uri: &str) -> Result<Vec<PublishDiagnostics>, String> {
        self.check_open_document(uri)
    }

    pub fn close_document(&mut self, uri: &str) -> Vec<PublishDiagnostics> {
        self.documents.close(uri);
        if self.published_uris.remove(uri) {
            vec![PublishDiagnostics {
                uri: uri.to_string(),
                diagnostics: vec![],
            }]
        } else {
            vec![]
        }
    }

    pub fn check_open_document(&mut self, uri: &str) -> Result<Vec<PublishDiagnostics>, String> {
        let document = self
            .documents
            .get(uri)
            .ok_or_else(|| format!("document '{uri}' is not open"))?;
        let path = document
            .path
            .clone()
            .ok_or_else(|| format!("document '{uri}' is not a file URI"))?;
        let sources = crate::frontend_sources::source_bundle()?;
        let input = CheckFileInput::new(path, sources)
            .map_err(|error| error.to_string())?
            .with_source_overrides(self.documents.source_overrides()?);
        let report = match anvyx_lang2::check_file(input) {
            Ok(ok) => ok.report,
            Err(error) => match error.report() {
                Some(report) => report.clone(),
                None => return Err(error.to_string()),
            },
        };
        Ok(self.publish_report(&report))
    }

    pub fn publish_report(&mut self, report: &DiagnosticReport) -> Vec<PublishDiagnostics> {
        let mut publishes = diagnostics_by_uri(report, &self.documents);
        let current = publishes
            .iter()
            .map(|publish| publish.uri.clone())
            .collect::<HashSet<_>>();
        for uri in self.published_uris.difference(&current) {
            publishes.push(PublishDiagnostics {
                uri: uri.clone(),
                diagnostics: vec![],
            });
        }
        self.published_uris = current;
        publishes.sort_by(|left, right| left.uri.cmp(&right.uri));
        publishes
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct DocumentStore {
    documents: HashMap<String, Document>,
}

impl DocumentStore {
    pub fn open(&mut self, uri: impl Into<String>, version: Option<i32>, text: impl Into<String>) {
        let uri = uri.into();
        let path = uri_to_path(&uri);
        self.documents.insert(
            uri.clone(),
            Document {
                uri,
                version,
                text: text.into(),
                path,
            },
        );
    }

    pub fn change(&mut self, uri: &str, version: Option<i32>, text: impl Into<String>) {
        if let Some(document) = self.documents.get_mut(uri) {
            document.version = version;
            document.text = text.into();
        }
    }

    pub fn close(&mut self, uri: &str) {
        self.documents.remove(uri);
    }

    pub fn get(&self, uri: &str) -> Option<&Document> {
        self.documents.get(uri)
    }

    pub fn source_overrides(&self) -> Result<Vec<SourceOverride>, String> {
        self.documents
            .values()
            .filter_map(|document| {
                document
                    .path
                    .as_ref()
                    .map(|path| (path, document.text.as_str()))
            })
            .map(|(path, text)| SourceOverride::new(path.clone(), text.to_string()))
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| error.to_string())
    }

    fn uri_for_source(&self, path: &Path) -> String {
        match self
            .documents
            .values()
            .find(|document| document.path_matches(path))
        {
            Some(document) => document.uri.clone(),
            None => path_to_uri(path),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Document {
    uri: String,
    version: Option<i32>,
    text: String,
    path: Option<PathBuf>,
}

impl Document {
    fn path_matches(&self, path: &Path) -> bool {
        let Some(document_path) = &self.path else {
            return false;
        };
        same_path(document_path, path)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PublishDiagnostics {
    pub uri: String,
    pub diagnostics: Vec<LspDiagnostic>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LspDiagnostic {
    pub range: LspRange,
    pub severity: u8,
    pub message: String,
    #[serde(rename = "relatedInformation", skip_serializing_if = "Vec::is_empty")]
    pub related_information: Vec<LspRelatedInformation>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct LspRange {
    pub start: LspPosition,
    pub end: LspPosition,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct LspPosition {
    pub line: u32,
    pub character: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LspRelatedInformation {
    pub location: LspLocation,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct LspLocation {
    pub uri: String,
    pub range: LspRange,
}

pub fn diagnostics_by_uri(
    report: &DiagnosticReport,
    documents: &DocumentStore,
) -> Vec<PublishDiagnostics> {
    let mut diagnostics = HashMap::<String, Vec<LspDiagnostic>>::new();
    for diagnostic in report.diagnostics() {
        let Some(primary) = report.anchor_label(diagnostic) else {
            continue;
        };
        let Some(file) = report.source(primary.span.source()) else {
            continue;
        };
        let Some(uri) = file.path().map(|path| documents.uri_for_source(path)) else {
            continue;
        };
        let Some(range) = label_range(report, primary) else {
            continue;
        };
        diagnostics.entry(uri).or_default().push(LspDiagnostic {
            range,
            severity: lsp_severity(diagnostic.severity()),
            message: lsp_message(diagnostic, primary),
            related_information: related_information(report, documents, diagnostic, primary),
        });
    }

    let mut publishes = diagnostics
        .into_iter()
        .map(|(uri, diagnostics)| PublishDiagnostics { uri, diagnostics })
        .collect::<Vec<_>>();
    publishes.sort_by(|left, right| left.uri.cmp(&right.uri));
    publishes
}

fn related_information(
    report: &DiagnosticReport,
    documents: &DocumentStore,
    diagnostic: &Diagnostic,
    anchor: &DiagnosticLabel,
) -> Vec<LspRelatedInformation> {
    diagnostic
        .labels()
        .iter()
        .filter(|label| label.style == LabelStyle::Secondary && !std::ptr::eq(*label, anchor))
        .filter_map(|label| {
            let file = report.source(label.span.source())?;
            let uri = documents.uri_for_source(file.path()?);
            let range = label_range(report, label)?;
            Some(LspRelatedInformation {
                location: LspLocation { uri, range },
                message: label.message.clone().unwrap_or_default(),
            })
        })
        .collect()
}

fn label_range(report: &DiagnosticReport, label: &DiagnosticLabel) -> Option<LspRange> {
    let file = report.source(label.span.source())?;
    let start = file.line_index().byte_to_lsp_position(label.span.start())?;
    let end = file.line_index().byte_to_lsp_position(label.span.end())?;
    Some(LspRange {
        start: LspPosition {
            line: start.line,
            character: start.character,
        },
        end: LspPosition {
            line: end.line,
            character: end.character,
        },
    })
}

fn lsp_severity(severity: DiagnosticSeverity) -> u8 {
    match severity {
        DiagnosticSeverity::Error => 1,
        DiagnosticSeverity::Warning => 2,
    }
}

fn lsp_message(diagnostic: &Diagnostic, anchor: &DiagnosticLabel) -> String {
    let mut message = diagnostic.message().to_string();
    if let Some(label) = anchor.message.as_deref()
        && label != diagnostic.message()
    {
        message.push_str(": ");
        message.push_str(label);
    }
    for note in diagnostic.notes() {
        message.push_str("\nnote: ");
        message.push_str(note);
    }
    if let Some(help) = diagnostic.help() {
        message.push_str("\nhelp: ");
        message.push_str(help);
    }
    message
}

fn uri_to_path(uri: &str) -> Option<PathBuf> {
    let raw = uri
        .strip_prefix("file://localhost")
        .or_else(|| uri.strip_prefix("file://"))?;
    percent_decode(raw).map(PathBuf::from)
}

fn path_to_uri(path: &Path) -> String {
    format!("file://{}", percent_encode_path(&path.to_string_lossy()))
}

fn percent_decode(raw: &str) -> Option<String> {
    let bytes = raw.as_bytes();
    let mut decoded = Vec::with_capacity(bytes.len());
    let mut index = 0;
    while index < bytes.len() {
        match bytes[index] {
            b'%' => {
                let high = hex_value(*bytes.get(index + 1)?)?;
                let low = hex_value(*bytes.get(index + 2)?)?;
                decoded.push((high << 4) | low);
                index += 3;
            }
            byte => {
                decoded.push(byte);
                index += 1;
            }
        }
    }
    String::from_utf8(decoded).ok()
}

fn hex_value(byte: u8) -> Option<u8> {
    match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        b'A'..=b'F' => Some(byte - b'A' + 10),
        _ => None,
    }
}

fn percent_encode_path(path: &str) -> String {
    let mut encoded = String::with_capacity(path.len());
    for byte in path.bytes() {
        if is_uri_path_byte(byte) {
            encoded.push(byte as char);
        } else {
            const HEX: &[u8; 16] = b"0123456789ABCDEF";
            encoded.push('%');
            encoded.push(HEX[(byte >> 4) as usize] as char);
            encoded.push(HEX[(byte & 0x0f) as usize] as char);
        }
    }
    encoded
}

fn is_uri_path_byte(byte: u8) -> bool {
    matches!(
        byte,
        b'A'..=b'Z'
            | b'a'..=b'z'
            | b'0'..=b'9'
            | b'-'
            | b'.'
            | b'_'
            | b'~'
            | b'/'
            | b':'
    )
}

fn same_path(left: &Path, right: &Path) -> bool {
    comparable_path(left) == comparable_path(right)
}

fn comparable_path(path: &Path) -> PathBuf {
    match path.canonicalize() {
        Ok(path) => path,
        Err(error) if error.kind() == io::ErrorKind::NotFound => {
            missing_path(path).unwrap_or_else(|| normalize_path(path))
        }
        Err(_) => normalize_path(path),
    }
}

fn missing_path(path: &Path) -> Option<PathBuf> {
    let absolute = absolute_path(path).ok()?;
    let mut missing = Vec::<OsString>::new();
    let mut cursor = absolute.as_path();

    loop {
        match cursor.canonicalize() {
            Ok(mut existing) => {
                for component in missing.iter().rev() {
                    existing.push(component);
                }
                return Some(existing);
            }
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                missing.push(cursor.file_name()?.to_os_string());
                cursor = cursor.parent()?;
            }
            Err(_) => return None,
        }
    }
}

fn absolute_path(path: &Path) -> io::Result<PathBuf> {
    if path.is_absolute() {
        return Ok(normalize_path(path));
    }
    let path = std::env::current_dir()?.join(path);
    Ok(normalize_path(&path))
}

fn normalize_path(path: &Path) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                normalized.pop();
            }
            other => normalized.push(other.as_os_str()),
        }
    }
    normalized
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, Cursor};

    use anvyx_lang2::{DiagnosticReport, SourceId, SourceKind, SourceSpan, SourceTable};

    use super::*;

    fn frame(message: Value) -> Vec<u8> {
        let body = serde_json::to_vec(&message).unwrap();
        let mut framed = format!("Content-Length: {}\r\n\r\n", body.len()).into_bytes();
        framed.extend(body);
        framed
    }

    fn read_output(output: Vec<u8>) -> Vec<Value> {
        let mut reader = BufReader::new(Cursor::new(output));
        let mut messages = vec![];
        while let Some(message) = read_message(&mut reader).unwrap() {
            messages.push(message);
        }
        messages
    }

    fn report(
        path: PathBuf,
        text: &str,
        make: impl FnOnce(SourceId) -> Diagnostic,
    ) -> DiagnosticReport {
        let mut sources = SourceTable::default();
        let source = sources.add(
            SourceKind::Root,
            path.display().to_string(),
            Some(path),
            text,
        );
        DiagnosticReport {
            sources,
            diagnostics: vec![make(source)],
        }
    }

    fn path_uri(path: &Path) -> String {
        path_to_uri(path)
    }

    #[test]
    fn server_responds_to_initialize() {
        let input = frame(json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {}
        }));
        let mut output = Vec::new();

        run_server(&mut BufReader::new(Cursor::new(input)), &mut output).unwrap();
        let messages = read_output(output);

        assert_eq!(messages[0]["id"], 1);
        assert!(messages[0]["result"]["capabilities"]["textDocumentSync"].is_object());
    }

    #[test]
    fn server_publishes_diagnostics_on_open() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        std::fs::write(&path, "fn main() {}").unwrap();
        let uri = path_uri(&path);
        let input = frame(json!({
            "jsonrpc": "2.0",
            "method": "textDocument/didOpen",
            "params": {
                "textDocument": {
                    "uri": uri,
                    "version": 1,
                    "text": "fn main() { let x: int = true; }"
                }
            }
        }));
        let mut output = Vec::new();

        run_server(&mut BufReader::new(Cursor::new(input)), &mut output).unwrap();
        let messages = read_output(output);

        assert_eq!(messages[0]["method"], "textDocument/publishDiagnostics");
        assert!(
            !messages[0]["params"]["diagnostics"]
                .as_array()
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn document_store_uses_open_text_before_disk() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        std::fs::write(&path, "disk").unwrap();
        let uri = path_uri(&path);
        let mut store = DocumentStore::default();
        store.open(uri, Some(1), "open");

        assert_eq!(store.source_overrides().unwrap()[0].code(), "open");
    }

    #[test]
    fn check_open_document_uses_open_text() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        std::fs::write(&path, "fn main() {}").unwrap();
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();
        let publishes = adapter
            .open_document(uri.clone(), Some(1), "fn main() { let x: int = true; }")
            .unwrap();

        assert_eq!(publishes[0].uri, uri);
        assert_eq!(publishes[0].diagnostics[0].severity, 1);
        assert_eq!(
            publishes[0].diagnostics[0].message,
            "Mismatched types: expected 'int', found 'bool'"
        );

        let clears = adapter
            .change_document(&uri, Some(2), "fn main() {}")
            .unwrap();
        assert!(clears[0].diagnostics.is_empty());
    }

    #[test]
    fn file_uri_paths_are_percent_encoded() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main file.anv");
        let uri = path_uri(&path);
        let mut store = DocumentStore::default();

        store.open(uri.clone(), Some(1), "fn main() {}");

        assert!(uri.contains("main%20file.anv"));
        assert_eq!(
            store.get(&uri).unwrap().path.as_deref(),
            Some(path.as_path())
        );
    }

    #[test]
    fn check_open_document_accepts_missing_file_override() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("new file.anv");
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();

        let publishes = adapter
            .open_document(uri.clone(), Some(1), "fn main() { let x: int = true; }")
            .unwrap();

        assert_eq!(publishes[0].uri, uri);
        assert_eq!(publishes[0].diagnostics[0].severity, 1);
    }

    #[test]
    fn converts_ascii_range() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "fn main() { bad; }";
        let report = report(path.clone(), text, |source| {
            Diagnostic::error("bad").with_primary(SourceSpan::new(source, 12, 15))
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&path), Some(1), text);

        let publishes = diagnostics_by_uri(&report, &store);

        assert_eq!(publishes[0].diagnostics[0].range.start.line, 0);
        assert_eq!(publishes[0].diagnostics[0].range.start.character, 12);
        assert_eq!(publishes[0].diagnostics[0].range.end.character, 15);
    }

    #[test]
    fn secondary_anchor_maps_to_range_when_no_primary_exists() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "fn main() { bad; }";
        let report = report(path.clone(), text, |source| {
            Diagnostic::error("bad")
                .with_secondary_message(SourceSpan::new(source, 12, 15), "related")
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&path), Some(1), text);

        let diagnostic = diagnostics_by_uri(&report, &store)
            .remove(0)
            .diagnostics
            .remove(0);

        assert_eq!(diagnostic.range.start.character, 12);
        assert_eq!(diagnostic.range.end.character, 15);
        assert_eq!(diagnostic.message, "bad: related");
        assert!(diagnostic.related_information.is_empty());
    }

    #[test]
    fn includes_notes_and_help_in_message() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "fn main() { bad; }";
        let report = report(path.clone(), text, |source| {
            Diagnostic::error("bad")
                .with_primary(SourceSpan::new(source, 12, 15))
                .with_note("check this")
                .with_help("try that")
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&path), Some(1), text);

        let message = diagnostics_by_uri(&report, &store)
            .remove(0)
            .diagnostics
            .remove(0)
            .message;

        assert_eq!(message, "bad\nnote: check this\nhelp: try that");
    }

    #[test]
    fn converts_non_ascii_range() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "let café = 1;";
        let report = report(path.clone(), text, |source| {
            Diagnostic::error("bad").with_primary(SourceSpan::new(source, 4, 9))
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&path), Some(1), text);

        let range = diagnostics_by_uri(&report, &store).remove(0).diagnostics[0].range;

        assert_eq!(range.start.character, 4);
        assert_eq!(range.end.character, 8);
    }

    #[test]
    fn converts_emoji_before_range() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "🙂 bad";
        let report = report(path.clone(), text, |source| {
            Diagnostic::error("bad").with_primary(SourceSpan::new(source, 5, 8))
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&path), Some(1), text);

        let range = diagnostics_by_uri(&report, &store).remove(0).diagnostics[0].range;

        assert_eq!(range.start.character, 3);
        assert_eq!(range.end.character, 6);
    }

    #[test]
    fn maps_imported_module_diagnostic_uri() {
        let temp = tempfile::tempdir().unwrap();
        let main = temp.path().join("main.anv");
        let module = temp.path().join("helper.anv");
        let text = "bad";
        let report = report(module.clone(), text, |source| {
            Diagnostic::error("bad").with_primary(SourceSpan::new(source, 0, 3))
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&main), Some(1), "import helper;");
        store.open(path_uri(&module), Some(1), text);

        let publishes = diagnostics_by_uri(&report, &store);

        assert_eq!(publishes[0].uri, path_uri(&module));
    }

    #[test]
    fn converts_secondary_labels_to_related_information() {
        let temp = tempfile::tempdir().unwrap();
        let main = temp.path().join("main.anv");
        let helper = temp.path().join("helper.anv");
        let mut sources = SourceTable::default();
        let main_source = sources.add(SourceKind::Root, "main.anv", Some(main.clone()), "call();");
        let helper_source = sources.add(
            SourceKind::Virtual,
            "helper.anv",
            Some(helper.clone()),
            "fn call() {}",
        );
        let diagnostic = Diagnostic::error("bad call")
            .with_primary(SourceSpan::new(main_source, 0, 4))
            .with_secondary_message(SourceSpan::new(helper_source, 3, 7), "defined here");
        let report = DiagnosticReport {
            sources,
            diagnostics: vec![diagnostic],
        };
        let mut store = DocumentStore::default();
        store.open(path_uri(&main), Some(1), "call();");
        store.open(path_uri(&helper), Some(1), "fn call() {}");

        let related = diagnostics_by_uri(&report, &store)
            .remove(0)
            .diagnostics
            .remove(0)
            .related_information
            .remove(0);

        assert_eq!(related.location.uri, path_uri(&helper));
        assert_eq!(related.message, "defined here");
    }

    #[test]
    fn message_only_diagnostic_has_no_fake_publish() {
        let report = DiagnosticReport {
            sources: SourceTable::default(),
            diagnostics: vec![Diagnostic::error("provider failed")],
        };

        assert!(diagnostics_by_uri(&report, &DocumentStore::default()).is_empty());
    }

    #[test]
    fn publishes_empty_diagnostics_for_cleared_files() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "bad";
        let report = report(path.clone(), text, |source| {
            Diagnostic::error("bad").with_primary(SourceSpan::new(source, 0, 3))
        });
        let empty = DiagnosticReport::default();
        let mut adapter = LspAdapter::default();
        adapter.documents.open(path_uri(&path), Some(1), text);

        assert_eq!(adapter.publish_report(&report).len(), 1);
        let publishes = adapter.publish_report(&empty);

        assert_eq!(publishes[0].uri, path_uri(&path));
        assert!(publishes[0].diagnostics.is_empty());
    }
}
