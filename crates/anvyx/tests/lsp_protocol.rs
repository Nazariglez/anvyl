use std::{
    fs,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    process::{Child, ChildStdin, Command, Stdio},
    sync::mpsc::{self, Receiver},
    thread,
    time::Duration,
};

use serde_json::{Value, json};

struct Client {
    child: Child,
    input: Option<ChildStdin>,
    output: Receiver<Result<Value, String>>,
}

impl Client {
    fn start(root: &Path) -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_anvyx"))
            .current_dir(root)
            .arg("lsp")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        let stdout = child.stdout.take().unwrap();
        let (tx, output) = mpsc::channel();
        thread::spawn(move || {
            let mut stdout = BufReader::new(stdout);
            loop {
                let message = read_message(&mut stdout);
                let done = message.is_err();
                if tx.send(message).is_err() || done {
                    break;
                }
            }
        });
        Self {
            input: child.stdin.take(),
            output,
            child,
        }
    }

    fn send(&mut self, message: Value) {
        let body = serde_json::to_vec(&message).unwrap();
        let input = self.input.as_mut().unwrap();
        write!(input, "Content-Length: {}\r\n\r\n", body.len()).unwrap();
        input.write_all(&body).unwrap();
        input.flush().unwrap();
    }

    fn notify(&mut self, method: &str, params: Value) {
        self.send(json!({"jsonrpc":"2.0", "method": method, "params": params}));
    }

    fn receive(&self) -> Value {
        self.output
            .recv_timeout(Duration::from_secs(60))
            .expect("timed out waiting for LSP response")
            .expect("failed to read LSP response")
    }

    fn initialize(&mut self) {
        self.send(json!({"jsonrpc":"2.0", "id": 1, "method":"initialize", "params": {}}));
        let response = self.receive();
        assert_eq!(response["id"], 1);
        assert_eq!(
            response["result"]["capabilities"]["textDocumentSync"]["openClose"],
            true
        );
        assert_eq!(
            response["result"]["capabilities"]["textDocumentSync"]["change"],
            1
        );
        self.notify("initialized", json!({}));
    }

    fn shutdown(&mut self) {
        self.send(json!({"jsonrpc":"2.0", "id": 2, "method":"shutdown", "params": null}));
        assert_eq!(self.receive()["id"], 2);
        self.notify("exit", Value::Null);
        self.input.take();
        let _ = self.child.kill();
        let _ = self.child.wait();
    }

    fn open(&mut self, uri: &str, version: i32, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({"textDocument": {"uri": uri, "languageId":"anvyx", "version":version, "text":text}}),
        );
    }

    fn change(&mut self, uri: &str, version: i32, text: &str) {
        self.notify(
            "textDocument/didChange",
            json!({"textDocument": {"uri": uri, "version":version}, "contentChanges": [{"text":text}]}),
        );
    }

    fn publish(&mut self, uri: &str) -> Value {
        for _ in 0..8 {
            let publish = self.receive();
            assert_eq!(publish["method"], "textDocument/publishDiagnostics");
            let actual = publish["params"]["uri"].as_str().unwrap();
            if normalize_uri(actual) == normalize_uri(uri) {
                return publish["params"].clone();
            }
        }
        panic!("did not receive diagnostics for {uri}");
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        self.input.take();
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn read_message(reader: &mut impl BufRead) -> Result<Value, String> {
    let mut length = None;
    loop {
        let mut line = String::new();
        reader
            .read_line(&mut line)
            .map_err(|error| error.to_string())?;
        if line.is_empty() {
            return Err("LSP process closed its output".to_string());
        }
        if line == "\r\n" {
            break;
        }
        if let Some(value) = line.strip_prefix("Content-Length: ") {
            length = Some(
                value
                    .trim()
                    .parse::<usize>()
                    .map_err(|error| error.to_string())?,
            );
        }
    }
    let mut body = vec![0; length.ok_or("LSP message missing Content-Length")?];
    std::io::Read::read_exact(reader, &mut body).map_err(|error| error.to_string())?;
    serde_json::from_slice(&body).map_err(|error| error.to_string())
}

fn uri(path: &Path) -> String {
    format!("file://{}", path.display())
}

fn normalize_uri(uri: &str) -> String {
    let path = PathBuf::from(uri.strip_prefix("file://").unwrap().replace("%20", " "));
    let path = path.canonicalize().unwrap_or_else(|_| {
        path.parent()
            .and_then(|parent| parent.canonicalize().ok())
            .map(|parent| parent.join(path.file_name().unwrap()))
            .unwrap_or(path)
    });
    format!("file://{}", path.display()).replace(' ', "%20")
}

fn write(root: &Path, relative: &str, text: &str) {
    let path = root.join(relative);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).unwrap();
    }
    fs::write(path, text).unwrap();
}

#[test]
fn lsp_stdio_initializes_and_handles_shutdown() {
    let temp = tempfile::tempdir().unwrap();
    let mut client = Client::start(temp.path());
    client.initialize();
    client.shutdown();
}

#[test]
fn lsp_stdio_tracks_open_change_save_close_and_missing_documents() {
    let temp = tempfile::tempdir().unwrap();
    let main = temp.path().join("main.anv");
    write(temp.path(), "main.anv", "fn main() {}\n");
    let missing = temp.path().join("new file.anv");
    let mut client = Client::start(temp.path());
    client.initialize();

    let main_uri = uri(&main);
    client.open(&main_uri, 1, "fn main() { let x: int = true; }");
    let error = client.publish(&main_uri);
    assert_eq!(error["diagnostics"][0]["severity"], 1);

    client.change(&main_uri, 2, "fn main() {}");
    let clear = client.publish(&main_uri);
    assert!(clear["diagnostics"].as_array().unwrap().is_empty());

    client.change(&main_uri, 3, "fn main() { let x: int = true; }");
    let changed = client.publish(&main_uri);
    assert_eq!(changed["diagnostics"][0]["severity"], 1);

    client.notify(
        "textDocument/didSave",
        json!({"textDocument": {"uri": main_uri}}),
    );
    let saved = client.publish(&main_uri);
    assert_eq!(saved["diagnostics"][0]["severity"], 1);

    client.notify(
        "textDocument/didClose",
        json!({"textDocument": {"uri": main_uri}}),
    );
    let closed = client.publish(&main_uri);
    assert!(closed["diagnostics"].as_array().unwrap().is_empty());

    let missing_uri = uri(&missing);
    client.open(&missing_uri, 1, "fn main() { let x: int = true; }");
    let missing_error = client.publish(&missing_uri);
    assert_eq!(missing_error["diagnostics"][0]["severity"], 1);
}

#[test]
fn lsp_stdio_publishes_manifest_and_cross_file_diagnostics() {
    let temp = tempfile::tempdir().unwrap();
    write(
        temp.path(),
        "game/anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n\n[lint]\npublic_inferred_dyn_contract = \"error\"\n",
    );
    let main = temp.path().join("game/src/main.anv");
    let helper = temp.path().join("game/src/helper.anv");
    write(
        temp.path(),
        "game/src/main.anv",
        "import helper;\nstruct Actor { fn draw(self) {} }\npub fn take(actor: dyn _) { actor.draw(); }\n",
    );
    write(
        temp.path(),
        "game/src/helper.anv",
        "pub fn bad() { let x: int = true; }\n",
    );
    let mut client = Client::start(temp.path());
    client.initialize();

    let main_uri = uri(&main);
    client.open(
        &main_uri,
        1,
        "import helper;\nstruct Actor { fn draw(self) {} }\npub fn take(actor: dyn _) { actor.draw(); }\n",
    );
    let lint = client.publish(&main_uri);
    let diagnostic = &lint["diagnostics"][0];
    assert_eq!(diagnostic["code"], "public_inferred_dyn_contract");
    assert_eq!(diagnostic["severity"], 1);
    assert!(
        diagnostic["message"]
            .as_str()
            .unwrap()
            .contains("declare a named contract")
    );
    assert!(
        diagnostic["range"]["end"]["character"].as_u64().unwrap()
            > diagnostic["range"]["start"]["character"].as_u64().unwrap()
    );

    write(
        temp.path(),
        "game/anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n\n[lint]\nunused_variable = \"warn\"\n",
    );
    client.change(&main_uri, 2, "fn main() {}\n");
    let manifest_error = client.publish(&main_uri);
    assert!(
        manifest_error["diagnostics"][0]["message"]
            .as_str()
            .unwrap()
            .contains("unknown lint or group")
    );

    write(
        temp.path(),
        "game/anvyx.toml",
        "[project]\nentry = \"src/main.anv\"\n",
    );
    client.change(
        &main_uri,
        3,
        "import helper;\nfn main() { helper.bad(); }\n",
    );
    let helper_uri = uri(&helper);
    let imported = client.publish(&helper_uri);
    assert_eq!(imported["diagnostics"][0]["severity"], 1);

    let other = temp.path().join("game/src/other.anv");
    write(temp.path(), "game/src/other.anv", "fn main() {}\n");
    let other_uri = uri(&other);
    client.open(&other_uri, 1, "fn main() {}");
    client.change(&main_uri, 3, "fn main() {}");
    let cleared = client.publish(&helper_uri);
    assert!(cleared["diagnostics"].as_array().unwrap().is_empty());
}

#[test]
fn lsp_stdio_preserves_diagnostic_metadata_utf16_ranges_and_related_information() {
    let temp = tempfile::tempdir().unwrap();
    let main = temp.path().join("main.anv");
    write(temp.path(), "main.anv", "import unused;\nfn main() {}\n");
    write(temp.path(), "helper.anv", "pub fn value() {}\n");
    write(temp.path(), "unused.anv", "pub fn value() {}\n");
    let mut client = Client::start(temp.path());
    client.initialize();

    let main_uri = uri(&main);
    client.open(&main_uri, 1, "import unused;\nfn main() {}\n");
    let diagnostics = client.publish(&main_uri);
    let metadata = diagnostics["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|diagnostic| diagnostic["code"] == "unused_import")
        .unwrap();
    assert_eq!(metadata["source"], "anvyx");
    assert_eq!(metadata["severity"], 2);
    assert_eq!(metadata["tags"], json!([1]));
    assert!(
        metadata["message"]
            .as_str()
            .unwrap()
            .contains("help: remove this import")
    );

    client.change(
        &main_uri,
        2,
        "fn main() { println(\"🙂\"); let x: int = true; }\n",
    );
    let emoji = client.publish(&main_uri);
    let diagnostic = emoji["diagnostics"]
        .as_array()
        .unwrap()
        .iter()
        .find(|diagnostic| {
            diagnostic["message"]
                .as_str()
                .unwrap()
                .contains("mismatched types")
        })
        .unwrap();
    assert_eq!(
        diagnostic["range"],
        json!({
            "start": {"line": 0, "character": 40},
            "end": {"line": 0, "character": 44}
        })
    );

    client.change(
        &main_uri,
        3,
        "extern type Handle;\nextern type Handle;\nfn main() {}\n",
    );
    let related = client.publish(&main_uri);
    let related = &related["diagnostics"][0]["relatedInformation"][0];
    assert_eq!(
        normalize_uri(related["location"]["uri"].as_str().unwrap()),
        normalize_uri(&main_uri)
    );
    assert_eq!(related["message"], "first declared here");
}
