use std::{
    collections::{HashMap, HashSet},
    ffi::OsString,
    io,
    path::{Component, Path, PathBuf},
};

use anvyx_lang::{
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
        let output = match anvyx_project::check::check_path_with_manifest_lints(&path, overrides) {
            Ok(output) => output,
            Err(error) => return self.document_error(uri, version, error),
        };
        Ok(self.publish_report_for(uri, &output.report))
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
