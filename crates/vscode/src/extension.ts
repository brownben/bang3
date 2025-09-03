import { type ExtensionContext, window, workspace } from 'vscode'
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from 'vscode-languageclient/node'

const EXTENSION_NAME = 'Bang Language Server'
let client: LanguageClient

export function activate(_context: ExtensionContext) {
  const outputChannel = window.createOutputChannel(EXTENSION_NAME, {
    log: true,
  })
  outputChannel.clear()

  const config = workspace.getConfiguration('bang')
  const serverPath = config.get<string>('server.path') ?? 'bang'
  outputChannel.appendLine(`Starting Bang Language Server (${serverPath})`)

  const serverOptions: ServerOptions = {
    command: serverPath,
    args: ['lsp'],
  }
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: 'file', language: 'bang' },
      { scheme: 'untitled', language: 'bang' },
    ],
    outputChannel: outputChannel,
  }

  client = new LanguageClient(
    'bang',
    EXTENSION_NAME,
    serverOptions,
    clientOptions,
  )
  client.start()
}

export function deactivate() {
  if (client) {
    return client.stop()
  }
}
