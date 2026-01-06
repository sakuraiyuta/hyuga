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
import { existsSync, statSync } from 'fs';
import { platform } from 'os';
import { delimiter, dirname, resolve} from 'path';
import { OutputChannel } from 'vscode';
import { PythonExtension } from '@vscode/python-extension';

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

let channel: OutputChannel | null = null;

const windows: boolean = platform() == 'win32';
const hyuga_exe: string = windows ? 'hyuga.exe' : 'hyuga';

async function searchHyuga() {
  const pythonApi: PythonExtension = await PythonExtension.api();
  const environments = pythonApi.environments;
  await environments.refreshEnvironments();
  channel?.appendLine(`environments.known: ${JSON.stringify(environments.known)}`);
  const venvs = environments.known.filter(env => env.environment?.type == 'VirtualEnvironment'
  	&& (env.environment.folderUri && env.environment.folderUri.path)
  	&& (env.tools.some(v => v == 'Venv'))
  );
  for (const venv of venvs) {
    channel?.appendLine(`venv: ${JSON.stringify(venv)}`);
  	const fpath = venv.environment?.folderUri.fsPath;
    if (!fpath) {
      continue;
    }
  	const v_path = statSync(fpath).isFile() ? dirname(fpath) : fpath;
  	const h_path = resolve(v_path, hyuga_exe);
  	const a_path = resolve(v_path, 'activate');
  	channel?.appendLine(`folderUri.fsPath: ${fpath}`);
  	channel?.appendLine(`v_path: ${v_path}`);
  	channel?.appendLine(`hyuga: ${JSON.stringify(h_path)}`);
  	channel?.appendLine(`activate: ${JSON.stringify(a_path)}`);
 
  	if (!existsSync(a_path) || !statSync(a_path).isFile()) {
      channel?.appendLine(`activate doesn't exist: ${JSON.stringify(v_path)}`);
      continue;
  	}
  	if (existsSync(h_path) && statSync(h_path).isFile()) {
      return h_path;
  	}
  }

  const p = process.env['PATH']?.split(delimiter).find((x) => existsSync(resolve(x, hyuga_exe)));
  if (p) {
    return resolve(p, hyuga_exe);
  } else {
    vscode.window.showWarningMessage(`No ${hyuga_exe} found`);
    return hyuga_exe;
  }
}

// This method is called when your extension is activated
// Your extension is activated the very first time the command is executed
export async function activate(context: ExtensionContext) {
  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
 	channel = vscode.window.createOutputChannel('Hyuga Client');
	channel?.appendLine('hyuga-vscode-client activation...');

  try {
    const hyugaCmd = await searchHyuga();
    channel?.appendLine(`hyugaCmd: ${hyugaCmd}`);
    const serverOptions = {
      command: hyugaCmd,
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
export function deactivate() {
  if (client) {return client.stop();}
}
