use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    io,
    path::{Component, Path, PathBuf},
};

use anvyx_lang2::{
    Diagnostic as FrontendDiagnostic, DiagnosticLabel, DiagnosticReport,
    DiagnosticSeverity as FrontendDiagnosticSeverity, DiagnosticTag as FrontendDiagnosticTag,
    LabelStyle, SourceOverride,
};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    Diagnostic as LspDiagnostic, DiagnosticRelatedInformation, DiagnosticSeverity as LspSeverity,
    DiagnosticTag as LspTag, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, Location, NumberOrString, Position,
    PublishDiagnosticsParams, Range, Url,
};
use serde_json::{Value, json};

pub fn run_stdio() -> Result<(), String> {
    let (connection, threads) = Connection::stdio();
    connection
        .initialize(initialize_result())
        .map_err(|error| error.to_string())?;

    let mut adapter = LspAdapter::default();
    let mut shutdown = false;
    let mut sink = &connection;
    for message in &connection.receiver {
        if handle_lsp_message(&mut adapter, &mut sink, message, &mut shutdown)? {
            break;
        }
    }
    threads
        .join()
        .map_err(|error| format!("lsp io error: {error}"))
}

fn initialize_result() -> Value {
    json!({
        "textDocumentSync": {
            "openClose": true,
            "change": 1,
            "save": true
        }
    })
}

trait MessageSink {
    fn send(&mut self, message: Message) -> Result<(), String>;
}

impl MessageSink for &Connection {
    fn send(&mut self, message: Message) -> Result<(), String> {
        self.sender.send(message).map_err(|error| error.to_string())
    }
}

fn handle_lsp_message(
    adapter: &mut LspAdapter,
    sink: &mut impl MessageSink,
    message: Message,
    shutdown: &mut bool,
) -> Result<bool, String> {
    match message {
        Message::Request(request) => handle_lsp_request(sink, request, shutdown),
        Message::Response(_) => Ok(false),
        Message::Notification(notification) => {
            handle_lsp_notification(adapter, sink, notification, shutdown)
        }
    }
}

fn handle_lsp_request(
    sink: &mut impl MessageSink,
    request: Request,
    shutdown: &mut bool,
) -> Result<bool, String> {
    if request.method == "shutdown" {
        *shutdown = true;
    }
    sink.send(Message::Response(Response::new_ok(request.id, Value::Null)))?;
    Ok(false)
}

fn handle_lsp_notification(
    adapter: &mut LspAdapter,
    sink: &mut impl MessageSink,
    notification: Notification,
    shutdown: &mut bool,
) -> Result<bool, String> {
    match notification.method.as_str() {
        "exit" => Ok(*shutdown),
        "textDocument/didOpen" => {
            let params = serde_json::from_value::<DidOpenTextDocumentParams>(notification.params)
                .map_err(|error| format!("invalid didOpen params: {error}"))?;
            let document = params.text_document;
            send_publishes(
                sink,
                adapter.open_document(
                    document.uri.to_string(),
                    Some(document.version),
                    document.text,
                )?,
            )?;
            Ok(false)
        }
        "textDocument/didChange" => {
            let params = serde_json::from_value::<DidChangeTextDocumentParams>(notification.params)
                .map_err(|error| format!("invalid didChange params: {error}"))?;
            let text = params
                .content_changes
                .last()
                .ok_or_else(|| "didChange missing contentChanges entry".to_string())?
                .text
                .clone();
            send_publishes(
                sink,
                adapter.change_document(
                    params.text_document.uri.as_str(),
                    Some(params.text_document.version),
                    text,
                )?,
            )?;
            Ok(false)
        }
        "textDocument/didSave" => {
            let params = serde_json::from_value::<DidSaveTextDocumentParams>(notification.params)
                .map_err(|error| format!("invalid didSave params: {error}"))?;
            send_publishes(
                sink,
                adapter.save_document(params.text_document.uri.as_str())?,
            )?;
            Ok(false)
        }
        "textDocument/didClose" => {
            let params = serde_json::from_value::<DidCloseTextDocumentParams>(notification.params)
                .map_err(|error| format!("invalid didClose params: {error}"))?;
            send_publishes(
                sink,
                adapter.close_document(params.text_document.uri.as_str()),
            )?;
            Ok(false)
        }
        _ => Ok(false),
    }
}

fn send_publishes(
    sink: &mut impl MessageSink,
    publishes: Vec<PublishDiagnosticsParams>,
) -> Result<(), String> {
    for publish in publishes {
        sink.send(Message::Notification(Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            serde_json::to_value(publish).map_err(|error| error.to_string())?,
        )))?;
    }
    Ok(())
}

#[derive(Debug, Default)]
pub struct LspAdapter {
    documents: DocumentStore,
    published_uris: HashMap<String, HashSet<Url>>,
}

impl LspAdapter {
    pub fn open_document(
        &mut self,
        uri: impl Into<String>,
        version: Option<i32>,
        text: impl Into<String>,
    ) -> Result<Vec<PublishDiagnosticsParams>, String> {
        let uri = uri.into();
        self.documents.open(uri.clone(), version, text);
        self.check_open_document(&uri)
    }

    pub fn change_document(
        &mut self,
        uri: &str,
        version: Option<i32>,
        text: impl Into<String>,
    ) -> Result<Vec<PublishDiagnosticsParams>, String> {
        self.documents.change(uri, version, text);
        self.check_open_document(uri)
    }

    pub fn save_document(&mut self, uri: &str) -> Result<Vec<PublishDiagnosticsParams>, String> {
        self.check_open_document(uri)
    }

    pub fn close_document(&mut self, uri: &str) -> Vec<PublishDiagnosticsParams> {
        self.documents.close(uri);
        let stale = self.published_uris.remove(uri).unwrap_or_default();
        stale
            .into_iter()
            .filter(|uri| !self.is_published_in_other_scope(uri))
            .map(|uri| PublishDiagnosticsParams {
                uri,
                diagnostics: vec![],
                version: None,
            })
            .collect()
    }

    pub fn check_open_document(
        &mut self,
        uri: &str,
    ) -> Result<Vec<PublishDiagnosticsParams>, String> {
        let document = self
            .documents
            .get(uri)
            .ok_or_else(|| format!("document '{uri}' is not open"))?;
        let version = document.version;
        let Some(path) = document.path.clone() else {
            return self.document_error(
                uri,
                version,
                format!("document '{uri}' is not a file URI"),
            );
        };
        let overrides = match self.documents.source_overrides() {
            Ok(overrides) => overrides,
            Err(error) => return self.document_error(uri, version, error),
        };
        let result = match anvyx_project::check::check_path_with_manifest_lints(&path, overrides) {
            Ok(result) => result,
            Err(error) => return self.document_error(uri, version, error),
        };
        let report = match result {
            Ok(ok) => ok.report,
            Err(error) => match error.report() {
                Some(report) => report.clone(),
                None => return self.document_error(uri, version, error.to_string()),
            },
        };
        Ok(self.publish_report_for(uri, &report))
    }

    fn document_error(
        &mut self,
        scope: &str,
        version: Option<i32>,
        message: String,
    ) -> Result<Vec<PublishDiagnosticsParams>, String> {
        let uri = Url::parse(scope).map_err(|error| error.to_string())?;
        Ok(self.publish_error_for(scope, uri, version, message))
    }

    fn publish_error_for(
        &mut self,
        scope: &str,
        uri: Url,
        version: Option<i32>,
        message: String,
    ) -> Vec<PublishDiagnosticsParams> {
        let stale = self.replace_published_scope(scope, HashSet::from([uri.clone()]));
        let mut publishes = vec![PublishDiagnosticsParams {
            uri,
            diagnostics: vec![LspDiagnostic {
                range: Range::default(),
                severity: Some(LspSeverity::ERROR),
                source: Some("anvyx".to_string()),
                message,
                ..LspDiagnostic::default()
            }],
            version,
        }];
        self.push_stale_clears(&mut publishes, stale);
        publishes.sort_by(|left, right| left.uri.cmp(&right.uri));
        publishes
    }

    fn publish_report_for(
        &mut self,
        scope: &str,
        report: &DiagnosticReport,
    ) -> Vec<PublishDiagnosticsParams> {
        let mut publishes = diagnostics_by_uri(report, &self.documents);
        let current = publishes
            .iter()
            .map(|publish| publish.uri.clone())
            .collect::<HashSet<_>>();
        let stale = self.replace_published_scope(scope, current);
        self.push_stale_clears(&mut publishes, stale);
        publishes.sort_by(|left, right| left.uri.cmp(&right.uri));
        publishes
    }

    fn replace_published_scope(&mut self, scope: &str, current: HashSet<Url>) -> HashSet<Url> {
        self.published_uris
            .insert(scope.to_string(), current)
            .unwrap_or_default()
    }

    fn push_stale_clears(
        &self,
        publishes: &mut Vec<PublishDiagnosticsParams>,
        stale: HashSet<Url>,
    ) {
        for uri in stale {
            if !self.is_published_in_other_scope(&uri) {
                publishes.push(PublishDiagnosticsParams {
                    uri,
                    diagnostics: vec![],
                    version: None,
                });
            }
        }
    }

    fn is_published_in_other_scope(&self, uri: &Url) -> bool {
        self.published_uris
            .values()
            .any(|published| published.contains(uri))
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

    fn uri_for_source(&self, path: &Path) -> Option<Url> {
        match self
            .documents
            .values()
            .find(|document| document.path_matches(path))
        {
            Some(document) => Url::parse(&document.uri).ok(),
            None => Some(path_to_uri(path)),
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

fn diagnostics_by_uri(
    report: &DiagnosticReport,
    documents: &DocumentStore,
) -> Vec<PublishDiagnosticsParams> {
    let mut diagnostics = HashMap::<Url, Vec<LspDiagnostic>>::new();
    for diagnostic in report.diagnostics() {
        let Some(primary) = report.anchor_label(diagnostic) else {
            continue;
        };
        let Some(file) = report.source(primary.span.source()) else {
            continue;
        };
        let Some(uri) = file.path().and_then(|path| documents.uri_for_source(path)) else {
            continue;
        };
        let Some(range) = label_range(report, primary) else {
            continue;
        };
        let tags = diagnostic
            .tags()
            .iter()
            .copied()
            .map(lsp_tag)
            .collect::<Vec<_>>();
        let related_information = related_information(report, documents, diagnostic, primary);
        diagnostics.entry(uri).or_default().push(LspDiagnostic {
            range,
            severity: Some(lsp_severity(diagnostic.severity())),
            source: diagnostic.code().map(|code| code.source.to_string()),
            code: diagnostic
                .code()
                .map(|code| NumberOrString::String(code.code.clone())),
            tags: (!tags.is_empty()).then_some(tags),
            message: lsp_message(diagnostic, primary),
            related_information: (!related_information.is_empty()).then_some(related_information),
            ..LspDiagnostic::default()
        });
    }

    let mut publishes = diagnostics
        .into_iter()
        .map(|(uri, diagnostics)| PublishDiagnosticsParams {
            uri,
            diagnostics,
            version: None,
        })
        .collect::<Vec<_>>();
    publishes.sort_by(|left, right| left.uri.cmp(&right.uri));
    publishes
}

fn related_information(
    report: &DiagnosticReport,
    documents: &DocumentStore,
    diagnostic: &FrontendDiagnostic,
    anchor: &DiagnosticLabel,
) -> Vec<DiagnosticRelatedInformation> {
    diagnostic
        .labels()
        .iter()
        .filter(|label| label.style == LabelStyle::Secondary && !std::ptr::eq(*label, anchor))
        .filter_map(|label| {
            let file = report.source(label.span.source())?;
            let uri = documents.uri_for_source(file.path()?)?;
            let range = label_range(report, label)?;
            Some(DiagnosticRelatedInformation {
                location: Location { uri, range },
                message: label.message.clone().unwrap_or_default(),
            })
        })
        .collect()
}

fn label_range(report: &DiagnosticReport, label: &DiagnosticLabel) -> Option<Range> {
    let file = report.source(label.span.source())?;
    let start = file.line_index().byte_to_lsp_position(label.span.start())?;
    let end = file.line_index().byte_to_lsp_position(label.span.end())?;
    Some(Range {
        start: Position {
            line: start.line,
            character: start.character,
        },
        end: Position {
            line: end.line,
            character: end.character,
        },
    })
}

fn lsp_severity(severity: FrontendDiagnosticSeverity) -> LspSeverity {
    match severity {
        FrontendDiagnosticSeverity::Error => LspSeverity::ERROR,
        FrontendDiagnosticSeverity::Warning => LspSeverity::WARNING,
    }
}

fn lsp_tag(tag: FrontendDiagnosticTag) -> LspTag {
    match tag {
        FrontendDiagnosticTag::Deprecated => LspTag::DEPRECATED,
        FrontendDiagnosticTag::Unnecessary => LspTag::UNNECESSARY,
    }
}

fn lsp_message(diagnostic: &FrontendDiagnostic, anchor: &DiagnosticLabel) -> String {
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
    Url::parse(uri).ok()?.to_file_path().ok()
}

fn path_to_uri(path: &Path) -> Url {
    Url::from_file_path(path).expect("absolute file path converts to URI")
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
    use anvyx_lang2::{DiagnosticReport, SourceId, SourceKind, SourceSpan, SourceTable};

    use super::*;

    #[derive(Default)]
    struct TestSink {
        messages: Vec<Message>,
    }

    impl MessageSink for TestSink {
        fn send(&mut self, message: Message) -> Result<(), String> {
            self.messages.push(message);
            Ok(())
        }
    }

    fn report(
        path: PathBuf,
        text: &str,
        make: impl FnOnce(SourceId) -> FrontendDiagnostic,
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
        path_to_uri(path).to_string()
    }

    #[test]
    fn initialize_result_advertises_full_text_sync() {
        let result = initialize_result();

        assert_eq!(result["textDocumentSync"]["openClose"], true);
        assert_eq!(result["textDocumentSync"]["change"], 1);
        assert_eq!(result["textDocumentSync"]["save"], true);
    }

    #[test]
    fn lsp_dispatch_responds_to_shutdown_and_exit() {
        let mut adapter = LspAdapter::default();
        let mut sink = TestSink::default();
        let mut shutdown = false;

        let done = handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Request(Request {
                id: 1.into(),
                method: "shutdown".to_string(),
                params: Value::Null,
            }),
            &mut shutdown,
        )
        .unwrap();
        assert!(!done);
        assert!(shutdown);
        assert!(matches!(sink.messages[0], Message::Response(_)));

        let done = handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Notification(Notification::new("exit".to_string(), Value::Null)),
            &mut shutdown,
        )
        .unwrap();
        assert!(done);
    }

    #[test]
    fn lsp_dispatch_publishes_diagnostics_on_open() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        std::fs::write(&path, "fn main() {}").unwrap();
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();
        let mut sink = TestSink::default();
        let mut shutdown = false;

        handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Notification(Notification::new(
                "textDocument/didOpen".to_string(),
                json!({
                    "textDocument": {
                        "uri": uri,
                        "languageId": "anvyx",
                        "version": 1,
                        "text": "fn main() { let x: int = true; }"
                    }
                }),
            )),
            &mut shutdown,
        )
        .unwrap();

        let Message::Notification(notification) = &sink.messages[0] else {
            panic!("expected publish notification");
        };
        assert_eq!(notification.method, "textDocument/publishDiagnostics");
        assert!(
            !notification.params["diagnostics"]
                .as_array()
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn lsp_dispatch_handles_change_save_and_close() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        std::fs::write(&path, "fn main() {}").unwrap();
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();
        let mut sink = TestSink::default();
        let mut shutdown = false;

        handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Notification(Notification::new(
                "textDocument/didOpen".to_string(),
                json!({ "textDocument": { "uri": uri, "languageId": "anvyx", "version": 1, "text": "fn main() {}" } }),
            )),
            &mut shutdown,
        )
        .unwrap();
        sink.messages.clear();

        handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Notification(Notification::new(
                "textDocument/didChange".to_string(),
                json!({
                    "textDocument": { "uri": uri, "version": 2 },
                    "contentChanges": [{ "text": "fn main() { let x: int = true; }" }]
                }),
            )),
            &mut shutdown,
        )
        .unwrap();
        let Message::Notification(change_publish) = &sink.messages[0] else {
            panic!("expected change publish");
        };
        assert!(
            !change_publish.params["diagnostics"]
                .as_array()
                .unwrap()
                .is_empty()
        );
        sink.messages.clear();

        handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Notification(Notification::new(
                "textDocument/didSave".to_string(),
                json!({ "textDocument": { "uri": uri } }),
            )),
            &mut shutdown,
        )
        .unwrap();
        assert_eq!(sink.messages.len(), 1);
        sink.messages.clear();

        handle_lsp_message(
            &mut adapter,
            &mut sink,
            Message::Notification(Notification::new(
                "textDocument/didClose".to_string(),
                json!({ "textDocument": { "uri": uri } }),
            )),
            &mut shutdown,
        )
        .unwrap();
        let Message::Notification(close_publish) = &sink.messages[0] else {
            panic!("expected close publish");
        };
        assert!(
            close_publish.params["diagnostics"]
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

        assert_eq!(publishes[0].uri.to_string(), uri);
        assert_eq!(
            publishes[0].diagnostics[0].severity,
            Some(LspSeverity::ERROR)
        );
        assert_eq!(
            publishes[0].diagnostics[0].message,
            "mismatched types: expected `int`, found `bool`"
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

        assert_eq!(publishes[0].uri.to_string(), uri);
        assert_eq!(
            publishes[0].diagnostics[0].severity,
            Some(LspSeverity::ERROR)
        );
    }

    #[test]
    fn check_open_document_publishes_unused_import_metadata() {
        let temp = tempfile::tempdir().unwrap();
        let main = temp.path().join("main.anv");
        let helper = temp.path().join("helper.anv");
        std::fs::write(&main, "import helper; fn main() {}\n").unwrap();
        std::fs::write(&helper, "pub fn f() {}\n").unwrap();
        let uri = path_uri(&main);
        let mut adapter = LspAdapter::default();

        let publishes = adapter
            .open_document(uri, Some(1), "import helper; fn main() {}")
            .unwrap();
        let diagnostic = &publishes[0].diagnostics[0];

        assert_eq!(diagnostic.source.as_deref(), Some("anvyx"));
        assert_eq!(
            diagnostic.code,
            Some(NumberOrString::String("unused_import".to_string()))
        );
        assert_eq!(diagnostic.tags, Some(vec![LspTag::UNNECESSARY]));
    }

    #[test]
    fn check_open_document_uses_nearest_manifest_lints() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("game");
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(
            root.join("anvyx.toml"),
            "[project]\nentry = \"src/main.anv\"\n\n[lint]\npublic_inferred_dyn_contract = \"error\"\n",
        )
        .unwrap();
        let path = root.join("src/main.anv");
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();

        let code = "struct Actor { fn draw(self) {} } pub fn take(actor: dyn _) { actor.draw(); }";
        let publishes = adapter.open_document(uri, Some(1), code).unwrap();
        let diagnostic = &publishes[0].diagnostics[0];
        let dyn_start = code.find("dyn _").unwrap() as u32;

        assert_eq!(diagnostic.severity, Some(LspSeverity::ERROR));
        assert_eq!(
            diagnostic.code,
            Some(NumberOrString::String(
                "public_inferred_dyn_contract".to_string()
            ))
        );
        assert_eq!(diagnostic.range.start.character, dyn_start);
        assert_eq!(diagnostic.range.end.character, dyn_start + 5);
        assert!(
            diagnostic
                .message
                .contains("inferred dynamic contract in exported API")
        );
        assert!(
            diagnostic
                .message
                .contains("declare a named contract and use `dyn Name`")
        );
        assert!(
            !diagnostic
                .message
                .contains("lint `public_inferred_dyn_contract`")
        );
    }

    #[test]
    fn check_open_document_publishes_manifest_lint_errors() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("game");
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(
            root.join("anvyx.toml"),
            "[project]\nentry = \"src/main.anv\"\n\n[lint]\nunused_variable = \"warn\"\n",
        )
        .unwrap();
        let path = root.join("src/main.anv");
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();

        let publishes = adapter
            .open_document(uri.clone(), Some(1), "fn main() {}")
            .unwrap();

        assert_eq!(publishes[0].uri.to_string(), uri);
        assert_eq!(
            publishes[0].diagnostics[0].severity,
            Some(LspSeverity::ERROR)
        );
        assert!(
            publishes[0].diagnostics[0]
                .message
                .contains("unknown lint or group 'unused_variable'"),
            "{}",
            publishes[0].diagnostics[0].message
        );
    }

    #[test]
    fn check_open_document_publishes_unsupported_extern_manifest() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("game");
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(
            root.join("anvyx.toml"),
            "[project]\nentry = \"src/main.anv\"\n\n[externs.engine]\npath = \"externs/engine\"\n",
        )
        .unwrap();
        let path = root.join("src/main.anv");
        let uri = path_uri(&path);
        let mut adapter = LspAdapter::default();

        let publishes = adapter
            .open_document(uri.clone(), Some(1), "fn main() {}")
            .unwrap();

        assert_eq!(publishes[0].uri.to_string(), uri);
        assert_eq!(
            publishes[0].diagnostics[0].severity,
            Some(LspSeverity::ERROR)
        );
        assert_eq!(
            publishes[0].diagnostics[0].message,
            "clean frontend does not support extern providers yet"
        );
    }

    #[test]
    fn check_open_document_publishes_imported_module_diagnostics() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("game");
        std::fs::create_dir_all(root.join("src")).unwrap();
        std::fs::write(
            root.join("anvyx.toml"),
            "[project]\nentry = \"src/main.anv\"\n",
        )
        .unwrap();
        let main = root.join("src/main.anv");
        let helper = root.join("src/helper.anv");
        std::fs::write(&main, "import helper; fn main() { helper.bad(); }\n").unwrap();
        std::fs::write(&helper, "pub fn bad() { let x: int = true; }\n").unwrap();
        let mut adapter = LspAdapter::default();

        let publishes = adapter
            .open_document(
                path_uri(&main),
                Some(1),
                "import helper; fn main() { helper.bad(); }",
            )
            .unwrap();

        assert!(publishes.iter().any(|publish| {
            publish
                .uri
                .to_file_path()
                .is_ok_and(|path| same_path(&path, &helper))
        }));
    }

    #[test]
    fn checking_clean_document_does_not_clear_unrelated_open_document() {
        let temp = tempfile::tempdir().unwrap();
        let first = temp.path().join("first.anv");
        let second = temp.path().join("second.anv");
        std::fs::write(&first, "fn main() {}\n").unwrap();
        std::fs::write(&second, "fn main() {}\n").unwrap();
        let mut adapter = LspAdapter::default();

        let first_publishes = adapter
            .open_document(
                path_uri(&first),
                Some(1),
                "fn main() { let x: int = true; }",
            )
            .unwrap();
        let second_publishes = adapter
            .open_document(path_uri(&second), Some(1), "fn main() {}")
            .unwrap();

        assert!(!first_publishes[0].diagnostics.is_empty());
        assert!(second_publishes.is_empty());
    }

    #[test]
    fn converts_ascii_range() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "fn main() { bad; }";
        let report = report(path.clone(), text, |source| {
            FrontendDiagnostic::error("bad").with_primary(SourceSpan::new(source, 12, 15))
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
            FrontendDiagnostic::error("bad")
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
        assert_eq!(diagnostic.source, None);
        assert_eq!(diagnostic.code, None);
        assert_eq!(diagnostic.tags, None);
        assert_eq!(diagnostic.message, "bad: related");
        assert_eq!(diagnostic.related_information, None);
    }

    #[test]
    fn includes_metadata() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "fn main() { bad; }";
        let report = report(path.clone(), text, |source| {
            FrontendDiagnostic::warning("bad")
                .with_code("anvyx", "deprecated")
                .with_tag(FrontendDiagnosticTag::Deprecated)
                .with_tag(FrontendDiagnosticTag::Unnecessary)
                .with_primary(SourceSpan::new(source, 12, 15))
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&path), Some(1), text);

        let diagnostic = diagnostics_by_uri(&report, &store)
            .remove(0)
            .diagnostics
            .remove(0);

        assert_eq!(diagnostic.severity, Some(LspSeverity::WARNING));
        assert_eq!(diagnostic.source.as_deref(), Some("anvyx"));
        assert_eq!(
            diagnostic.code,
            Some(NumberOrString::String("deprecated".to_string()))
        );
        assert_eq!(
            diagnostic.tags,
            Some(vec![LspTag::DEPRECATED, LspTag::UNNECESSARY])
        );
    }

    #[test]
    fn includes_notes_and_help_in_message() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "fn main() { bad; }";
        let report = report(path.clone(), text, |source| {
            FrontendDiagnostic::error("bad")
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
            FrontendDiagnostic::error("bad").with_primary(SourceSpan::new(source, 4, 9))
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
            FrontendDiagnostic::error("bad").with_primary(SourceSpan::new(source, 5, 8))
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
            FrontendDiagnostic::error("bad").with_primary(SourceSpan::new(source, 0, 3))
        });
        let mut store = DocumentStore::default();
        store.open(path_uri(&main), Some(1), "import helper;");
        store.open(path_uri(&module), Some(1), text);

        let publishes = diagnostics_by_uri(&report, &store);

        assert_eq!(publishes[0].uri.to_string(), path_uri(&module));
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
        let diagnostic = FrontendDiagnostic::error("bad call")
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
            .unwrap()
            .remove(0);

        assert_eq!(related.location.uri.to_string(), path_uri(&helper));
        assert_eq!(related.message, "defined here");
    }

    #[test]
    fn message_only_diagnostic_has_no_fake_publish() {
        let report = DiagnosticReport {
            sources: SourceTable::default(),
            diagnostics: vec![FrontendDiagnostic::error("provider failed")],
        };

        assert!(diagnostics_by_uri(&report, &DocumentStore::default()).is_empty());
    }

    #[test]
    fn publishes_empty_diagnostics_for_cleared_files() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("main.anv");
        let text = "bad";
        let report = report(path.clone(), text, |source| {
            FrontendDiagnostic::error("bad").with_primary(SourceSpan::new(source, 0, 3))
        });
        let empty = DiagnosticReport::default();
        let mut adapter = LspAdapter::default();
        adapter.documents.open(path_uri(&path), Some(1), text);

        assert_eq!(adapter.publish_report_for("", &report).len(), 1);
        let publishes = adapter.publish_report_for("", &empty);

        assert_eq!(publishes[0].uri.to_string(), path_uri(&path));
        assert!(publishes[0].diagnostics.is_empty());
    }
}
