import * as vscode from 'vscode';
import * as path from 'path';
import * as os from 'os';

import * as lc from 'vscode-languageclient/node';

let client: lc.LanguageClient;
let outputChannel: vscode.OutputChannel;

export function activate(context: vscode.ExtensionContext) {
	const statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 0);
	statusBarItem.text = "ark-server";
	statusBarItem.show();

	let disposable = vscode.commands.registerCommand('ark-lang.ping', () => {
		vscode.window.showInformationMessage("Action from Ark extension.");
	});

	context.subscriptions.push(disposable);

	const config = vscode.workspace.getConfiguration("ark.languageServer");
	const serverPath: string | undefined = config.get("path");

	if (serverPath) {
	    createClient(serverPath, statusBarItem);
	} else {
		vscode.window.showInformationMessage("Configure path to server in settings.");
	}
}

function createClient(serverPath: string, statusBarItem: vscode.StatusBarItem) {
	const serverOptions: lc.ServerOptions = { command: serverPath };

	outputChannel = vscode.window.createOutputChannel("Ark Language Server");

	const clientOptions: lc.LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'ark' }],
		outputChannel,
		revealOutputChannelOn: lc.RevealOutputChannelOn.Info,
	};

	client = new lc.LanguageClient('Ark Language Server', serverOptions, clientOptions, true);
	client.start();

	client.onDidChangeState((state) => {
		if (state.newState === lc.State.Running) {
			statusBarItem.text = "ark-server";
		} else if (state.newState === lc.State.Starting) {
			statusBarItem.text = "ark-server (starting)";
		} else if (state.newState === lc.State.Stopped) {
			statusBarItem.text = "ark-server (stopped)";
		}
	});
}

export function deactivate(): Thenable<void> | undefined {
	if (client) {
		return client.stop();
	} else {
		return undefined;
	}
}