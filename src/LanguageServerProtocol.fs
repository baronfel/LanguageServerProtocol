namespace Ionide.LanguageServerProtocol


open Newtonsoft.Json.Linq
open System.Diagnostics
open Ionide.LanguageServerProtocol.LspJsonConverters
open Ionide.LanguageServerProtocol.Types

module LowLevel =
  open System
  open System.IO
  open System.Text

  let headerBufferSize = 300
  let minimumHeaderLength = 21
  let cr = byte '\r'
  let lf = byte '\f'
  let headerEncoding = Encoding.ASCII

  let private readLine (stream: Stream) =
    let buffer = Array.zeroCreate<byte> headerBufferSize
    let readCount = stream.Read(buffer, 0, 2)
    let mutable count = readCount

    if count < 2 then
      None
    else
      // TODO: Check that we don't over-fill headerBufferSize
      while count < headerBufferSize
            && (buffer.[count - 2] <> cr && buffer.[count - 1] <> lf) do
        let additionalBytesRead = stream.Read(buffer, count, 1)
        // TODO: exit when additionalBytesRead = 0, end of stream
        count <- count + additionalBytesRead

      if count >= headerBufferSize then
        None
      else
        Some(headerEncoding.GetString(buffer, 0, count - 2))

  let rec private readHeaders (stream: Stream) =
    let line = readLine stream

    match line with
    | Some "" -> []
    | Some line ->
      let separatorPos = line.IndexOf(": ")

      if separatorPos = -1 then
        raise (Exception(sprintf "Separator not found in header '%s'" line))
      else
        let name = line.Substring(0, separatorPos)
        let value = line.Substring(separatorPos + 2)
        let otherHeaders = readHeaders stream
        (name, value) :: otherHeaders
    | None -> raise (EndOfStreamException())

  let read (stream: Stream) =
    let headers = readHeaders stream

    let contentLength =
      headers
      |> List.tryFind (fun (name, _) -> name = "Content-Length")
      |> Option.map snd
      |> Option.bind (fun s ->
        match Int32.TryParse(s) with
        | true, x -> Some x
        | _ -> None)

    if contentLength = None then
      failwithf "Content-Length header not found"
    else
      let result = Array.zeroCreate<byte> contentLength.Value
      let mutable readCount = 0

      while readCount < contentLength.Value do
        let toRead = contentLength.Value - readCount
        let readInCurrentBatch = stream.Read(result, readCount, toRead)
        readCount <- readCount + readInCurrentBatch

      let str = Encoding.UTF8.GetString(result, 0, readCount)
      headers, str

  let write (stream: Stream) (data: string) =
    let bytes = Encoding.UTF8.GetBytes(data)

    let header =
      sprintf "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: %d\r\n\r\n" bytes.Length

    let headerBytes = Encoding.ASCII.GetBytes header
    use ms = new MemoryStream(headerBytes.Length + bytes.Length)
    ms.Write(headerBytes, 0, headerBytes.Length)
    ms.Write(bytes, 0, bytes.Length)
    stream.Write(ms.ToArray(), 0, int ms.Position)

module JsonRpc =
  open Newtonsoft.Json
  open Newtonsoft.Json.Linq

  type MessageTypeTest =
    { [<JsonProperty("jsonrpc")>]
      Version: string
      Id: int option
      Method: string option }

  [<RequireQualifiedAccess>]
  type MessageType =
    | Notification
    | Request
    | Response
    | Error

  let getMessageType messageTest =
    match messageTest with
    | { Version = "2.0"; Id = Some _; Method = Some _ } -> MessageType.Request
    | { Version = "2.0"; Id = Some _; Method = None } -> MessageType.Response
    | { Version = "2.0"; Id = None; Method = Some _ } -> MessageType.Notification
    | _ -> MessageType.Error

  type Request =
    { [<JsonProperty("jsonrpc")>]
      Version: string
      Id: int
      Method: string
      Params: JToken option }
    static member Create(id: int, method': string, rpcParams: JToken option) =
      { Version = "2.0"; Id = id; Method = method'; Params = rpcParams }

  type Notification =
    { [<JsonProperty("jsonrpc")>]
      Version: string
      Method: string
      Params: JToken option }
    static member Create(method': string, rpcParams: JToken option) =
      { Version = "2.0"; Method = method'; Params = rpcParams }

  module ErrorCodes =
    let parseError = -32700
    let invalidRequest = -32600
    let methodNotFound = -32601
    let invalidParams = -32602
    let internalError = -32603
    let serverErrorStart = -32000
    let serverErrorEnd = -32099

  type Error =
    { Code: int
      Message: string
      Data: JToken option }
    static member Create(code: int, message: string) = { Code = code; Message = message; Data = None }

    static member ParseError = Error.Create(ErrorCodes.parseError, "Parse error")
    static member InvalidRequest = Error.Create(ErrorCodes.invalidRequest, "Invalid Request")
    static member MethodNotFound = Error.Create(ErrorCodes.methodNotFound, "Method not found")
    static member InvalidParams = Error.Create(ErrorCodes.invalidParams, "Invalid params")
    static member InternalError = Error.Create(ErrorCodes.internalError, "Internal error")

    static member InternalErrorMessage message = Error.Create(ErrorCodes.internalError, message)

  type Response =
    { [<JsonProperty("jsonrpc")>]
      Version: string
      Id: int option
      Error: Error option
      [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
      Result: JToken option }
    /// Json.NET conditional property serialization, controlled by naming convention
    member x.ShouldSerializeResult() = x.Error.IsNone

    static member Success(id: int, result: JToken option) =
      { Version = "2.0"; Id = Some id; Result = result; Error = None }

    static member Failure(id: int, error: Error) = { Version = "2.0"; Id = Some id; Result = None; Error = Some error }

type LspResult<'t> = Result<'t, JsonRpc.Error>
type AsyncLspResult<'t> = Async<LspResult<'t>>

module LspResult =
  let success x : LspResult<_> = Result.Ok x

  let invalidParams s : LspResult<_> = Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, s))

  let internalError<'a> (s: string) : LspResult<'a> =
    Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, s))

  let notImplemented<'a> : LspResult<'a> = Result.Error(JsonRpc.Error.MethodNotFound)

module AsyncLspResult =
  let success x : AsyncLspResult<_> = async.Return(Result.Ok x)

  let invalidParams s : AsyncLspResult<_> =
    async.Return(Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.invalidParams, s)))

  let internalError s : AsyncLspResult<_> =
    async.Return(Result.Error(JsonRpc.Error.Create(JsonRpc.ErrorCodes.internalError, s)))

  let notImplemented<'a> : AsyncLspResult<'a> = async.Return(Result.Error(JsonRpc.Error.MethodNotFound))

[<AutoOpen>]
module private Utils =
  /// Return the JSON-RPC "not implemented" error
  let notImplemented<'t> = async.Return LspResult.notImplemented<'t>

  /// Do nothing and ignore the notification
  let ignoreNotification = async.Return(())

[<AbstractClass>]
type LspClient() =
  /// The show message notification is sent from a server to a client to ask the client to display
  /// a particular message in the user interface.
  abstract member WindowShowMessage: ShowMessageParams -> Async<unit>

  default __.WindowShowMessage(_) = ignoreNotification

  /// The show message request is sent from a server to a client to ask the client to display
  /// a particular message in the user interface. In addition to the show message notification the
  /// request allows to pass actions and to wait for an answer from the client.
  abstract member WindowShowMessageRequest: ShowMessageRequestParams -> AsyncLspResult<MessageActionItem option>

  default __.WindowShowMessageRequest(_) = notImplemented

  /// The log message notification is sent from the server to the client to ask the client to log
  ///a particular message.
  abstract member WindowLogMessage: LogMessageParams -> Async<unit>

  default __.WindowLogMessage(_) = ignoreNotification

  /// The telemetry notification is sent from the server to the client to ask the client to log
  /// a telemetry event.
  abstract member TelemetryEvent: Newtonsoft.Json.Linq.JToken -> Async<unit>

  default __.TelemetryEvent(_) = ignoreNotification

  /// The `client/registerCapability` request is sent from the server to the client to register for a new
  /// capability on the client side. Not all clients need to support dynamic capability registration.
  /// A client opts in via the dynamicRegistration property on the specific client capabilities. A client
  /// can even provide dynamic registration for capability A but not for capability B.
  abstract member ClientRegisterCapability: RegistrationParams -> AsyncLspResult<unit>

  default __.ClientRegisterCapability(_) = notImplemented

  /// The `client/unregisterCapability` request is sent from the server to the client to unregister a previously
  /// registered capability.
  abstract member ClientUnregisterCapability: UnregistrationParams -> AsyncLspResult<unit>

  default __.ClientUnregisterCapability(_) = notImplemented

  /// Many tools support more than one root folder per workspace. Examples for this are VS Code’s multi-root
  /// support, Atom’s project folder support or Sublime’s project support. If a client workspace consists of
  /// multiple roots then a server typically needs to know about this. The protocol up to know assumes one root
  /// folder which is announce to the server by the rootUri property of the InitializeParams.
  /// If the client supports workspace folders and announces them via the corresponding workspaceFolders client
  /// capability the InitializeParams contain an additional property workspaceFolders with the configured
  /// workspace folders when the server starts.
  ///
  /// The workspace/workspaceFolders request is sent from the server to the client to fetch the current open
  /// list of workspace folders. Returns null in the response if only a single file is open in the tool.
  /// Returns an empty array if a workspace is open but no folders are configured.
  abstract member WorkspaceWorkspaceFolders: unit -> AsyncLspResult<WorkspaceFolder [] option>

  default __.WorkspaceWorkspaceFolders() = notImplemented

  /// The workspace/configuration request is sent from the server to the client to fetch configuration
  /// settings from the client.
  ///
  /// The request can fetch n configuration settings in one roundtrip. The order of the returned configuration
  /// settings correspond to the order of the passed ConfigurationItems (e.g. the first item in the response
  /// is the result for the first configuration item in the params).
  abstract member WorkspaceConfiguration: ConfigurationParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken []>

  default __.WorkspaceConfiguration(_) = notImplemented

  abstract member WorkspaceApplyEdit: ApplyWorkspaceEditParams -> AsyncLspResult<ApplyWorkspaceEditResponse>
  default __.WorkspaceApplyEdit(_) = notImplemented

  /// The workspace/semanticTokens/refresh request is sent from the server to the client.
  /// Servers can use it to ask clients to refresh the editors for which this server provides semantic tokens.
  /// As a result the client should ask the server to recompute the semantic tokens for these editors.
  /// This is useful if a server detects a project wide configuration change which requires a re-calculation
  /// of all semantic tokens. Note that the client still has the freedom to delay the re-calculation of
  /// the semantic tokens if for example an editor is currently not visible.
  abstract member WorkspaceSemanticTokensRefresh: unit -> Async<unit>

  default __.WorkspaceSemanticTokensRefresh() = ignoreNotification

  /// Diagnostics notification are sent from the server to the client to signal results of validation runs.
  ///
  /// Diagnostics are “owned” by the server so it is the server’s responsibility to clear them if necessary.
  /// The following rule is used for VS Code servers that generate diagnostics:
  ///
  /// * if a language is single file only (for example HTML) then diagnostics are cleared by the server when
  ///   the file is closed.
  /// * if a language has a project system (for example C#) diagnostics are not cleared when a file closes.
  ///   When a project is opened all diagnostics for all files are recomputed (or read from a cache).
  ///
  /// When a file changes it is the server’s responsibility to re-compute diagnostics and push them to the
  /// client. If the computed set is empty it has to push the empty array to clear former diagnostics.
  /// Newly pushed diagnostics always replace previously pushed diagnostics. There is no merging that happens
  /// on the client side.
  abstract member TextDocumentPublishDiagnostics: PublishDiagnosticsParams -> Async<unit>

  default __.TextDocumentPublishDiagnostics(_) = ignoreNotification



[<AbstractClass>]
type LspServer() =
  interface System.IDisposable with
    member x.Dispose() = x.Dispose()

  abstract member Dispose: unit -> unit

  /// The initialize request is sent as the first request from the client to the server.
  /// The initialize request may only be sent once.
  abstract member Initialize: InitializeParams -> AsyncLspResult<InitializeResult>

  default __.Initialize(_) = notImplemented

  /// The initialized notification is sent from the client to the server after the client received the result
  /// of the initialize request but before the client is sending any other request or notification to the server.
  /// The server can use the initialized notification for example to dynamically register capabilities.
  /// The initialized notification may only be sent once.
  abstract member Initialized: InitializedParams -> Async<unit>

  default __.Initialized(_) = ignoreNotification

  /// The shutdown request is sent from the client to the server. It asks the server to shut down, but to not
  /// exit (otherwise the response might not be delivered correctly to the client). There is a separate exit
  /// notification that asks the server to exit.
  abstract member Shutdown: unit -> Async<unit>

  default __.Shutdown() = ignoreNotification

  /// A notification to ask the server to exit its process.
  abstract member Exit: unit -> Async<unit>
  default __.Exit() = ignoreNotification

  /// The hover request is sent from the client to the server to request hover information at a given text
  /// document position.
  abstract member TextDocumentHover: TextDocumentPositionParams -> AsyncLspResult<Hover option>

  default __.TextDocumentHover(_) = notImplemented

  /// The document open notification is sent from the client to the server to signal newly opened text
  /// documents.
  ///
  /// The document’s truth is now managed by the client and the server must not try to read the document’s
  /// truth using the document’s uri. Open in this sense means it is managed by the client. It doesn't
  /// necessarily mean that its content is presented in an editor. An open notification must not be sent
  /// more than once without a corresponding close notification send before. This means open and close
  /// notification must be balanced and the max open count for a particular textDocument is one.
  abstract member TextDocumentDidOpen: DidOpenTextDocumentParams -> Async<unit>

  default __.TextDocumentDidOpen(_) = ignoreNotification

  /// The document change notification is sent from the client to the server to signal changes to a text document.
  abstract member TextDocumentDidChange: DidChangeTextDocumentParams -> Async<unit>
  default __.TextDocumentDidChange(_) = ignoreNotification

  /// The Completion request is sent from the client to the server to compute completion items at a given
  /// cursor position. Completion items are presented in the IntelliSense user interface.
  ///
  /// If computing full completion items is expensive, servers can additionally provide a handler for the
  /// completion item resolve request (‘completionItem/resolve’). This request is sent when a completion
  /// item is selected in the user interface. A typical use case is for example: the ‘textDocument/completion’
  /// request doesn’t fill in the documentation property for returned completion items since it is expensive
  /// to compute. When the item is selected in the user interface then a ‘completionItem/resolve’ request is
  /// sent with the selected completion item as a param. The returned completion item should have the
  /// documentation property filled in. The request can delay the computation of the detail and documentation
  /// properties. However, properties that are needed for the initial sorting and filtering, like sortText,
  /// filterText, insertText, and textEdit must be provided in the textDocument/completion request and must
  /// not be changed during resolve.
  abstract member TextDocumentCompletion: CompletionParams -> AsyncLspResult<CompletionList option>

  default __.TextDocumentCompletion(_) = notImplemented

  /// The request is sent from the client to the server to resolve additional information for a given
  /// completion item.
  abstract member CompletionItemResolve: CompletionItem -> AsyncLspResult<CompletionItem>

  default __.CompletionItemResolve(_) = notImplemented

  /// The rename request is sent from the client to the server to perform a workspace-wide rename of a symbol.
  abstract member TextDocumentRename: RenameParams -> AsyncLspResult<WorkspaceEdit option>
  default __.TextDocumentRename(_) = notImplemented

  /// The goto definition request is sent from the client to the server to resolve the definition location of
  /// a symbol at a given text document position.
  abstract member TextDocumentDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>

  default __.TextDocumentDefinition(_) = notImplemented

  /// The references request is sent from the client to the server to resolve project-wide references for
  /// the symbol denoted by the given text document position.
  abstract member TextDocumentReferences: ReferenceParams -> AsyncLspResult<Location [] option>

  default __.TextDocumentReferences(_) = notImplemented

  /// The document highlight request is sent from the client to the server to resolve a document highlights
  /// for a given text document position. For programming languages this usually highlights all references
  /// to the symbol scoped to this file.
  ///
  /// However we kept `textDocument/documentHighlight` and `textDocument/references` separate requests since
  /// the first one is allowed to be more fuzzy. Symbol matches usually have a DocumentHighlightKind of Read
  /// or Write whereas fuzzy or textual matches use Text as the kind.
  abstract member TextDocumentDocumentHighlight:
    TextDocumentPositionParams -> AsyncLspResult<DocumentHighlight [] option>

  default __.TextDocumentDocumentHighlight(_) = notImplemented

  /// The document links request is sent from the client to the server to request the location of links
  /// in a document.
  abstract member TextDocumentDocumentLink: DocumentLinkParams -> AsyncLspResult<DocumentLink [] option>

  default __.TextDocumentDocumentLink(_) = notImplemented

  /// The goto type definition request is sent from the client to the server to resolve the type definition
  /// location of a symbol at a given text document position.
  abstract member TextDocumentTypeDefinition: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>

  default __.TextDocumentTypeDefinition(_) = notImplemented

  /// The goto implementation request is sent from the client to the server to resolve the implementation
  /// location of a symbol at a given text document position.
  abstract member TextDocumentImplementation: TextDocumentPositionParams -> AsyncLspResult<GotoResult option>

  default __.TextDocumentImplementation(_) = notImplemented

  /// The code action request is sent from the client to the server to compute commands for a given text
  /// document and range. These commands are typically code fixes to either fix problems or to
  /// beautify/refactor code. The result of a textDocument/codeAction request is an array of Command literals
  /// which are typically presented in the user interface. When the command is selected the server should be
  /// contacted again (via the workspace/executeCommand) request to execute the command.
  abstract member TextDocumentCodeAction: CodeActionParams -> AsyncLspResult<TextDocumentCodeActionResult option>

  default __.TextDocumentCodeAction(_) = notImplemented

  /// The code action request is sent from the client to the server to compute commands for a given text
  /// document and range. These commands are typically code fixes to either fix problems or to
  /// beautify/refactor code. The result of a textDocument/codeAction request is an array of Command literals
  /// which are typically presented in the user interface. When the command is selected the server should be
  /// contacted again (via the workspace/executeCommand) request to execute the command.
  abstract member CodeActionResolve: CodeAction -> AsyncLspResult<CodeAction option>

  default __.CodeActionResolve(_) = notImplemented

  /// The code lens request is sent from the client to the server to compute code lenses for a given
  /// text document.
  abstract member TextDocumentCodeLens: CodeLensParams -> AsyncLspResult<CodeLens [] option>

  default __.TextDocumentCodeLens(_) = notImplemented

  /// The code lens resolve request is sent from the client to the server to resolve the command for
  /// a given code lens item.
  abstract member CodeLensResolve: CodeLens -> AsyncLspResult<CodeLens>

  default __.CodeLensResolve(_) = notImplemented

  /// The signature help request is sent from the client to the server to request signature information at
  /// a given cursor position.
  abstract member TextDocumentSignatureHelp: SignatureHelpParams -> AsyncLspResult<SignatureHelp option>

  default __.TextDocumentSignatureHelp(_) = notImplemented

  /// The document link resolve request is sent from the client to the server to resolve the target of
  /// a given document link.
  abstract member DocumentLinkResolve: DocumentLink -> AsyncLspResult<DocumentLink>

  default __.DocumentLinkResolve(_) = notImplemented

  /// The document color request is sent from the client to the server to list all color references
  /// found in a given text document. Along with the range, a color value in RGB is returned.
  abstract member TextDocumentDocumentColor: DocumentColorParams -> AsyncLspResult<ColorInformation []>

  default __.TextDocumentDocumentColor(_) = notImplemented

  /// The color presentation request is sent from the client to the server to obtain a list of
  /// presentations for a color value at a given location. Clients can use the result to
  abstract member TextDocumentColorPresentation: ColorPresentationParams -> AsyncLspResult<ColorPresentation []>

  default __.TextDocumentColorPresentation(_) = notImplemented

  /// The document formatting request is sent from the client to the server to format a whole document.
  abstract member TextDocumentFormatting: DocumentFormattingParams -> AsyncLspResult<TextEdit [] option>
  default __.TextDocumentFormatting(_) = notImplemented

  /// The document range formatting request is sent from the client to the server to format a given
  /// range in a document.
  abstract member TextDocumentRangeFormatting: DocumentRangeFormattingParams -> AsyncLspResult<TextEdit [] option>

  default __.TextDocumentRangeFormatting(_) = notImplemented

  /// The document on type formatting request is sent from the client to the server to format parts
  /// of the document during typing.
  abstract member TextDocumentOnTypeFormatting: DocumentOnTypeFormattingParams -> AsyncLspResult<TextEdit [] option>

  default __.TextDocumentOnTypeFormatting(_) = notImplemented

  /// The document symbol request is sent from the client to the server to return a flat list of all symbols
  /// found in a given text document. Neither the symbol’s location range nor the symbol’s container name
  /// should be used to infer a hierarchy.
  abstract member TextDocumentDocumentSymbol: DocumentSymbolParams -> AsyncLspResult<SymbolInformation [] option>

  default __.TextDocumentDocumentSymbol(_) = notImplemented

  /// The watched files notification is sent from the client to the server when the client detects changes
  /// to files watched by the language client. It is recommended that servers register for these file
  /// events using the registration mechanism. In former implementations clients pushed file events without
  /// the server actively asking for it.
  abstract member WorkspaceDidChangeWatchedFiles: DidChangeWatchedFilesParams -> Async<unit>

  default __.WorkspaceDidChangeWatchedFiles(_) = ignoreNotification

  /// The `workspace/didChangeWorkspaceFolders` notification is sent from the client to the server to inform
  /// the server about workspace folder configuration changes. The notification is sent by default if both
  /// *ServerCapabilities/workspace/workspaceFolders* and *ClientCapabilities/workapce/workspaceFolders* are
  /// true; or if the server has registered to receive this notification it first.
  abstract member WorkspaceDidChangeWorkspaceFolders: DidChangeWorkspaceFoldersParams -> Async<unit>

  default __.WorkspaceDidChangeWorkspaceFolders(_) = ignoreNotification

  /// A notification sent from the client to the server to signal the change of configuration settings.
  abstract member WorkspaceDidChangeConfiguration: DidChangeConfigurationParams -> Async<unit>
  default __.WorkspaceDidChangeConfiguration(_) = ignoreNotification

  /// The workspace symbol request is sent from the client to the server to list project-wide symbols matching
  /// the query string.
  abstract member WorkspaceSymbol: WorkspaceSymbolParams -> AsyncLspResult<SymbolInformation [] option>

  default __.WorkspaceSymbol(_) = notImplemented

  /// The `workspace/executeCommand` request is sent from the client to the server to trigger command execution
  /// on the server. In most cases the server creates a `WorkspaceEdit` structure and applies the changes to the
  /// workspace using the request `workspace/applyEdit` which is sent from the server to the client.
  abstract member WorkspaceExecuteCommand: ExecuteCommandParams -> AsyncLspResult<Newtonsoft.Json.Linq.JToken>

  default __.WorkspaceExecuteCommand(_) = notImplemented

  /// The document will save notification is sent from the client to the server before the document is
  /// actually saved.
  abstract member TextDocumentWillSave: WillSaveTextDocumentParams -> Async<unit>

  default __.TextDocumentWillSave(_) = ignoreNotification

  /// The document will save request is sent from the client to the server before the document is actually saved.
  /// The request can return an array of TextEdits which will be applied to the text document before it is saved.
  /// Please note that clients might drop results if computing the text edits took too long or if a server
  /// constantly fails on this request. This is done to keep the save fast and reliable.
  abstract member TextDocumentWillSaveWaitUntil: WillSaveTextDocumentParams -> AsyncLspResult<TextEdit [] option>

  default __.TextDocumentWillSaveWaitUntil(_) = notImplemented

  /// The document save notification is sent from the client to the server when the document was saved
  /// in the client.
  abstract member TextDocumentDidSave: DidSaveTextDocumentParams -> Async<unit>

  default __.TextDocumentDidSave(_) = ignoreNotification

  /// The document close notification is sent from the client to the server when the document got closed in the
  /// client. The document’s truth now exists where the document’s uri points to (e.g. if the document’s uri is
  /// a file uri the truth now exists on disk). As with the open notification the close notification is about
  /// managing the document’s content. Receiving a close notification doesn't mean that the document was open in
  /// an editor before. A close notification requires a previous open notification to be sent.
  abstract member TextDocumentDidClose: DidCloseTextDocumentParams -> Async<unit>

  default __.TextDocumentDidClose(_) = ignoreNotification

  /// The folding range request is sent from the client to the server to return all folding ranges found in a given text document.
  abstract member TextDocumentFoldingRange: FoldingRangeParams -> AsyncLspResult<FoldingRange list option>
  default __.TextDocumentFoldingRange(_) = notImplemented

  /// The selection range request is sent from the client to the server to return suggested selection ranges at an array of given positions.
  /// A selection range is a range around the cursor position which the user might be interested in selecting.
  abstract member TextDocumentSelectionRange: SelectionRangeParams -> AsyncLspResult<SelectionRange list option>

  default __.TextDocumentSelectionRange(_) = notImplemented

  abstract member TextDocumentSemanticTokensFull: SemanticTokensParams -> AsyncLspResult<SemanticTokens option>
  default __.TextDocumentSemanticTokensFull(_) = notImplemented

  abstract member TextDocumentSemanticTokensFullDelta:
    SemanticTokensDeltaParams -> AsyncLspResult<U2<SemanticTokens, SemanticTokensDelta> option>

  default __.TextDocumentSemanticTokensFullDelta(_) = notImplemented

  abstract member TextDocumentSemanticTokensRange: SemanticTokensRangeParams -> AsyncLspResult<SemanticTokens option>
  default __.TextDocumentSemanticTokensRange(_) = notImplemented

module Server =
  open System
  open System.IO
  open LanguageServerProtocol.Logging
  open System.Threading
  open System.Threading.Tasks
  open System.Reflection
  open StreamJsonRpc
  open Newtonsoft.Json
  open Newtonsoft.Json.Serialization

  let logger = LogProvider.getLoggerByName "LSP Server"

  let jsonRpcFormatter = new JsonMessageFormatter()
  jsonRpcFormatter.JsonSerializer.NullValueHandling <- NullValueHandling.Ignore
  jsonRpcFormatter.JsonSerializer.ConstructorHandling <- ConstructorHandling.AllowNonPublicDefaultConstructor
  jsonRpcFormatter.JsonSerializer.MissingMemberHandling <- MissingMemberHandling.Ignore
  jsonRpcFormatter.JsonSerializer.Converters.Add(SingleCaseUnionConverter())
  jsonRpcFormatter.JsonSerializer.Converters.Add(U2BoolObjectConverter())
  jsonRpcFormatter.JsonSerializer.Converters.Add(OptionConverter())
  jsonRpcFormatter.JsonSerializer.Converters.Add(ErasedUnionConverter())
  jsonRpcFormatter.JsonSerializer.ContractResolver <- CamelCasePropertyNamesContractResolver()

  let deserialize<'t> (token: JToken) = token.ToObject<'t>(jsonRpcFormatter.JsonSerializer)

  let serialize<'t> (o: 't) = JToken.FromObject(o, jsonRpcFormatter.JsonSerializer)

  let requestHandling<'param, 'result> (run: 'param -> AsyncLspResult<'result>) : Delegate =
    let runAsTask param ct =
      let asyncLspResult = run param

      let asyncContinuation =
        async {
          let! lspResult = asyncLspResult

          return
            match lspResult with
            | Ok result -> result
            | Error error ->
              let rpcException = LocalRpcException(error.Message)
              rpcException.ErrorCode <- error.Code
              rpcException.ErrorData <- error.Data |> Option.defaultValue null
              raise rpcException
        }

      Async.StartAsTask(asyncContinuation, cancellationToken = ct)

    Func<'param, CancellationToken, Task<'result>>(runAsTask) :> Delegate

  /// Notifications don't generate a response or error, but to unify things we consider them as always successful.
  /// They will still not send any response because their ID is null.
  let private notificationSuccess (response: Async<unit>) =
    async {
      do! response
      return Result.Ok()
    }

  type ClientNotificationSender = string -> obj -> AsyncLspResult<unit>

  type ClientRequestSender =
    abstract member Send<'a> : string -> obj -> AsyncLspResult<'a>

  type private MessageHandlingResult =
    | Normal
    | WasExit
    | WasShutdown

  type LspCloseReason =
    | RequestedByClient = 0
    | ErrorExitWithoutShutdown = 1
    | ErrorStreamClosed = 2

  let startWithSetup<'a when 'a :> LspClient>
    (setupRequestHandlings: 'a -> Map<string, Delegate>)
    (input: Stream)
    (output: Stream)
    (clientCreator: (ClientNotificationSender * ClientRequestSender) -> 'a)
    =

    use jsonRpcHandler = new HeaderDelimitedMessageHandler(output, input, jsonRpcFormatter)

    use jsonRpc = new JsonRpc(jsonRpcHandler)

    /// When the server wants to send a notification to the client
    let sendServerNotification (rpcMethod: string) (notificationObj: obj) : AsyncLspResult<unit> =
      async {
        do!
          jsonRpc.NotifyWithParameterObjectAsync(rpcMethod, notificationObj)
          |> Async.AwaitTask

        return () |> LspResult.success
      }

    /// When the server wants to send a request to the client
    let sendServerRequest (rpcMethod: string) (requestObj: obj) : AsyncLspResult<'response> =
      async {
        let! response =
          jsonRpc.InvokeWithParameterObjectAsync<'response>(rpcMethod, requestObj)
          |> Async.AwaitTask

        return response |> LspResult.success
      }

    let lspClient =
      clientCreator (
        sendServerNotification,
        { new ClientRequestSender with
            member __.Send x t = sendServerRequest x t }
      )

    let mutable shutdownReceived = false
    let mutable quitReceived = false
    use quitSemaphore = new SemaphoreSlim(0, 1)

    let onShutdown () = shutdownReceived <- true

    jsonRpc.AddLocalRpcMethod("shutdown", Action(onShutdown))

    let onExit () =
      quitReceived <- true
      quitSemaphore.Release() |> ignore

    jsonRpc.AddLocalRpcMethod("exit", Action(onExit))

    for handling in setupRequestHandlings lspClient do
      let rpcMethodName = handling.Key
      let rpcDelegate = handling.Value

      let rpcAttribute = JsonRpcMethodAttribute(rpcMethodName)
      rpcAttribute.UseSingleObjectParameterDeserialization <- true

      jsonRpc.AddLocalRpcMethod(rpcDelegate.GetMethodInfo(), rpcDelegate.Target, rpcAttribute)

    jsonRpc.StartListening()

    quitSemaphore.Wait()

    match shutdownReceived, quitReceived with
    | true, true -> LspCloseReason.RequestedByClient
    | false, true -> LspCloseReason.ErrorExitWithoutShutdown
    | _ -> LspCloseReason.ErrorStreamClosed

  type ServerRequestHandling<'server when 'server :> LspServer> = { Run: 'server -> Delegate }

  let serverRequestHandling<'server, 'param, 'result when 'server :> LspServer>
    (run: 'server -> 'param -> AsyncLspResult<'result>)
    : ServerRequestHandling<'server> =
    { Run = fun s -> requestHandling (run s) }

  let defaultRequestHandlings () : Map<string, ServerRequestHandling<'server>> =
    let requestHandling = serverRequestHandling

    [ "initialize", requestHandling (fun s p -> s.Initialize(p))
      "initialized", requestHandling (fun s p -> s.Initialized(p) |> notificationSuccess)
      "textDocument/hover", requestHandling (fun s p -> s.TextDocumentHover(p))
      "textDocument/didOpen", requestHandling (fun s p -> s.TextDocumentDidOpen(p) |> notificationSuccess)
      "textDocument/didChange", requestHandling (fun s p -> s.TextDocumentDidChange(p) |> notificationSuccess)
      "textDocument/completion", requestHandling (fun s p -> s.TextDocumentCompletion(p))
      "completionItem/resolve", requestHandling (fun s p -> s.CompletionItemResolve(p))
      "textDocument/rename", requestHandling (fun s p -> s.TextDocumentRename(p))
      "textDocument/definition", requestHandling (fun s p -> s.TextDocumentDefinition(p))
      "textDocument/typeDefinition", requestHandling (fun s p -> s.TextDocumentTypeDefinition(p))
      "textDocument/implementation", requestHandling (fun s p -> s.TextDocumentImplementation(p))
      "textDocument/codeAction", requestHandling (fun s p -> s.TextDocumentCodeAction(p))
      "codeAction/resolve", requestHandling (fun s p -> s.CodeActionResolve(p))
      "textDocument/codeLens", requestHandling (fun s p -> s.TextDocumentCodeLens(p))
      "codeLens/resolve", requestHandling (fun s p -> s.CodeLensResolve(p))
      "textDocument/references", requestHandling (fun s p -> s.TextDocumentReferences(p))
      "textDocument/documentHighlight", requestHandling (fun s p -> s.TextDocumentDocumentHighlight(p))
      "textDocument/documentLink", requestHandling (fun s p -> s.TextDocumentDocumentLink(p))
      "textDocument/signatureHelp", requestHandling (fun s p -> s.TextDocumentSignatureHelp(p))
      "documentLink/resolve", requestHandling (fun s p -> s.DocumentLinkResolve(p))
      "textDocument/documentColor", requestHandling (fun s p -> s.TextDocumentDocumentColor(p))
      "textDocument/colorPresentation", requestHandling (fun s p -> s.TextDocumentColorPresentation(p))
      "textDocument/formatting", requestHandling (fun s p -> s.TextDocumentFormatting(p))
      "textDocument/rangeFormatting", requestHandling (fun s p -> s.TextDocumentRangeFormatting(p))
      "textDocument/onTypeFormatting", requestHandling (fun s p -> s.TextDocumentOnTypeFormatting(p))
      "textDocument/willSave", requestHandling (fun s p -> s.TextDocumentWillSave(p) |> notificationSuccess)
      "textDocument/willSaveWaitUntil", requestHandling (fun s p -> s.TextDocumentWillSaveWaitUntil(p))
      "textDocument/didSave", requestHandling (fun s p -> s.TextDocumentDidSave(p) |> notificationSuccess)
      "textDocument/didClose", requestHandling (fun s p -> s.TextDocumentDidClose(p) |> notificationSuccess)
      "textDocument/documentSymbol", requestHandling (fun s p -> s.TextDocumentDocumentSymbol(p))
      "textDocument/foldingRange", requestHandling (fun s p -> s.TextDocumentFoldingRange(p))
      "textDocument/selectionRange", requestHandling (fun s p -> s.TextDocumentSelectionRange(p))
      "textDocument/semanticTokens/full", requestHandling (fun s p -> s.TextDocumentSemanticTokensFull(p))
      "textDocument/semanticTokens/full/delta", requestHandling (fun s p -> s.TextDocumentSemanticTokensFullDelta(p))
      "textDocument/semanticTokens/range", requestHandling (fun s p -> s.TextDocumentSemanticTokensRange(p))
      "workspace/didChangeWatchedFiles",
      requestHandling (fun s p -> s.WorkspaceDidChangeWatchedFiles(p) |> notificationSuccess)
      "workspace/didChangeWorkspaceFolders",
      requestHandling (fun s p ->
        s.WorkspaceDidChangeWorkspaceFolders(p)
        |> notificationSuccess)
      "workspace/didChangeConfiguration",
      requestHandling (fun s p -> s.WorkspaceDidChangeConfiguration(p) |> notificationSuccess)
      "workspace/symbol", requestHandling (fun s p -> s.WorkspaceSymbol(p))
      "workspace/executeCommand ", requestHandling (fun s p -> s.WorkspaceExecuteCommand(p))
      "shutdown", requestHandling (fun s () -> s.Shutdown() |> notificationSuccess)
      "exit", requestHandling (fun s () -> s.Exit() |> notificationSuccess) ]
    |> Map.ofList

  let start<'a, 'b when 'a :> LspClient and 'b :> LspServer>
    (requestHandlings: Map<string, Delegate>)
    (input: Stream)
    (output: Stream)
    (clientCreator: (ClientNotificationSender * ClientRequestSender) -> 'a)
    (serverCreator: 'a -> 'b)
    =
    let requestHandlingSetup _ = requestHandlings
    startWithSetup requestHandlingSetup

module Client =
  open System
  open System.IO
  open LanguageServerProtocol.Logging
  open Newtonsoft.Json
  open Newtonsoft.Json.Serialization

  open JsonRpc

  let logger = LogProvider.getLoggerByName "LSP Client"

  let internal jsonSettings =
    let result = JsonSerializerSettings(NullValueHandling = NullValueHandling.Ignore)
    result.Converters.Add(OptionConverter())
    result.Converters.Add(ErasedUnionConverter())
    result.ContractResolver <- CamelCasePropertyNamesContractResolver()
    result

  let internal jsonSerializer = JsonSerializer.Create(jsonSettings)

  let internal deserialize (token: JToken) = token.ToObject<'t>(jsonSerializer)

  let internal serialize (o: 't) = JToken.FromObject(o, jsonSerializer)

  type NotificationHandler = { Run: JToken -> Async<JToken option> }

  let notificationHandling<'p, 'r> (handler: 'p -> Async<'r option>) : NotificationHandler =
    let run (token: JToken) =
      async {
        try
          let p = token.ToObject<'p>(jsonSerializer)
          let! res = handler p

          return
            res
            |> Option.map (fun n -> JToken.FromObject(n, jsonSerializer))
        with
        | _ -> return None
      }

    { Run = run }

  type Client(exec: string, args: string, notificationHandlings: Map<string, NotificationHandler>) =

    let mutable outuptStream: StreamReader option = None
    let mutable inputStream: StreamWriter option = None

    let sender =
      MailboxProcessor<string>.Start
        (fun inbox ->
          let rec loop () =
            async {
              let! str = inbox.Receive()

              inputStream
              |> Option.iter (fun input ->
                // fprintfn stderr "[CLIENT] Writing: %s" str
                LowLevel.write input.BaseStream str
                input.BaseStream.Flush())
              // do! Async.Sleep 1000
              return! loop ()
            }

          loop ())

    let handleRequest (request: JsonRpc.Request) =
      async {
        let mutable methodCallResult = None

        match notificationHandlings |> Map.tryFind request.Method with
        | Some handling ->
          try
            match request.Params with
            | None -> ()
            | Some prms ->
              let! result = handling.Run prms
              methodCallResult <- result
          with
          | ex -> methodCallResult <- None
        | None -> ()

        match methodCallResult with
        | Some ok -> return Some(JsonRpc.Response.Success(request.Id, Some ok))
        | None -> return None
      }

    let handleNotification (notification: JsonRpc.Notification) =
      async {
        match notificationHandlings |> Map.tryFind notification.Method with
        | Some handling ->
          try
            match notification.Params with
            | None -> return Result.Error(Error.InvalidParams)
            | Some prms ->
              let! result = handling.Run prms
              return Result.Ok()
          with
          | ex -> return Result.Error(Error.Create(ErrorCodes.internalError, ex.ToString()))
        | None -> return Result.Error(Error.MethodNotFound)
      }

    let messageHanlder str =
      let messageTypeTest = JsonConvert.DeserializeObject<JsonRpc.MessageTypeTest>(str, jsonSettings)

      match getMessageType messageTypeTest with
      | MessageType.Notification ->
        let notification = JsonConvert.DeserializeObject<JsonRpc.Notification>(str, jsonSettings)

        async {
          let! result = handleNotification notification

          match result with
          | Result.Ok _ -> ()
          | Result.Error error ->
            logger.error (
              Log.setMessage "HandleServerMessage - Error {error} when handling notification {notification}"
              >> Log.addContextDestructured "error" error
              >> Log.addContextDestructured "notification" notification
            )
            //TODO: Handle error on receiving notification, send message to user?
            ()
        }
        |> Async.StartAsTask
        |> ignore
      | MessageType.Request ->
        let request = JsonConvert.DeserializeObject<JsonRpc.Request>(str, jsonSettings)

        async {
          let! result = handleRequest request

          match result with
          | Some response ->
            let responseString = JsonConvert.SerializeObject(response, jsonSettings)
            sender.Post(responseString)
          | None -> ()
        }
        |> Async.StartAsTask
        |> ignore
      | MessageType.Response
      | MessageType.Error ->
        logger.error (
          Log.setMessage "HandleServerMessage - Message had invalid jsonrpc version: {messageTypeTest}"
          >> Log.addContextDestructured "messageTypeTest" messageTypeTest
        )

        ()

      let request = JsonConvert.DeserializeObject<JsonRpc.Request>(str, jsonSettings)

      async {
        let! result = handleRequest request

        match result with
        | Some response ->
          let responseString = JsonConvert.SerializeObject(response, jsonSettings)
          sender.Post(responseString)
        | None -> ()
      }
      |> Async.StartAsTask
      |> ignore

    member __.SendNotification (rpcMethod: string) (requestObj: obj) =
      let serializedResponse = JToken.FromObject(requestObj, jsonSerializer)
      let notification = JsonRpc.Notification.Create(rpcMethod, Some serializedResponse)
      let notString = JsonConvert.SerializeObject(notification, jsonSettings)
      sender.Post(notString)

    member __.Start() =
      async {
        let si = ProcessStartInfo()
        si.RedirectStandardOutput <- true
        si.RedirectStandardInput <- true
        si.RedirectStandardError <- true
        si.UseShellExecute <- false
        si.WorkingDirectory <- Environment.CurrentDirectory
        si.FileName <- exec
        si.Arguments <- args

        let proc =
          try
            Process.Start(si)
          with
          | ex ->
            let newEx = System.Exception(sprintf "%s on %s" ex.Message exec, ex)
            raise newEx

        inputStream <- Some(proc.StandardInput)
        outuptStream <- Some(proc.StandardOutput)

        let mutable quit = false
        let outStream = proc.StandardOutput.BaseStream

        while not quit do
          try
            let _, notificationString = LowLevel.read outStream
            // fprintfn stderr "[CLIENT] READING: %s" notificationString
            messageHanlder notificationString
          with
          | :? EndOfStreamException -> quit <- true
          | ex -> ()

        return ()
      }
      |> Async.Start