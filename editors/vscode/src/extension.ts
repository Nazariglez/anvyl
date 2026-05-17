import * as vscode from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  const serverPath = getAnvyxCommand();
  const serverOptions: ServerOptions = {
    command: serverPath,
    args: ["lsp"],
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "anvyx" }],
    outputChannelName: "Anvyx Language Server",
    revealOutputChannelOn: RevealOutputChannelOn.Error,
  };

  client = new LanguageClient(
    "anvyx",
    "Anvyx Language Server",
    serverOptions,
    clientOptions,
  );
  context.subscriptions.push(client);

  client.start().catch((error: unknown) => {
    const message = error instanceof Error ? error.message : String(error);
    void vscode.window.showErrorMessage(
      `Failed to start Anvyx language server with '${serverPath} lsp'. Check anvyx.serverPath. ${message}`,
    );
  });
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}

function getAnvyxCommand(): string {
  const command = vscode.workspace
    .getConfiguration("anvyx")
    .get<string>("serverPath", "anvyx")
    .trim();
  return command || "anvyx";
}
