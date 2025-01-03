// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import type { ExtensionContext as ExtensionContext_vscode } from 'vscode';
import type {
    LanguageClient as LanguageClient_vscode,
} from 'vscode-languageclient/node';
import type {
    ExtensionContext as ExtensionContext_coc,
    LanguageClient as LanguageClient_coc,
} from 'coc.nvim';
type ExtensionContext = ExtensionContext_vscode | ExtensionContext_coc;
type LanguageClient = LanguageClient_vscode | LanguageClient_coc;
let vscode, vlc;
try {
    vscode = require('vscode');
    vlc = require('vscode-languageclient/node');
} catch (error) {
    vlc = require('coc.nvim');
    vscode = vlc;
}
const LanguageClient = vlc.LanguageClient;

let client: LanguageClient;
// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export function activate(context: ExtensionContext) {
  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
  console.info(`hyuga-vscode-client activation...`);

  try {
    const serverOptions = {
      command: "hyuga",
    };
    const clientOptions = {
      documentSelector: [
        {
          scheme: "file",
          language: "hy",
        }
      ],
    };
    client = new LanguageClient("hyuga", serverOptions, clientOptions);
    client.start();
  } catch (e) {
    vscode.window.showErrorMessage(`hyuga couldn't be started.\nerror=${e}`);
  }
}

// This method is called when your extension is deactivated
function deactivate() {
  if (client) {return client.stop();}
}

module.exports = { activate, deactivate };
