"use strict";
const vscode = require("vscode");
const languageclient = require("vscode-languageclient");

let client;

function activate(context) {
  try {
    const serverOptions = {
      command: "python3",
      args: ["-m", "hyuga"]
    };
    const clientOptions = {
      documentSelector: [
        {
          scheme: "file",
          language: "hy",
        }
      ],
    };
    client = new languageclient.LanguageClient("hyuga", serverOptions, clientOptions);
    context.subscriptions.push(client.start());
  } catch (e) {
    vscode.window.showErrorMessage("hyuga couldn't be started.");
  }
}

function deactivate() {
  if (client) return client.stop();
}

module.exports = { activate, deactivate }

