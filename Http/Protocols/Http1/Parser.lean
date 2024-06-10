import Http.Protocols.Http1.Data.Chunk
import Http.Protocols.Http1.Grammar
import Http.Data.Uri.Parser
import Http.Data.Headers
import Http.Data

namespace Http.Protocols.Http1

/-! This module handles HTTP/1.1 protocol parsing and state management. It includes functions for
handling URL, fields, and headers of an HTTP request.
-/

open Http.Protocols.Http1.Data
open Http.Data

/-- State structure to keep track of the request or repsonse that we are parsing right now -/
structure State where
  req: Request
  res: Response

  prop : String
  value : String

  contentLength : Option Nat
  hasHost : Bool
  isChunked : Bool
  uri: Uri.Parser

  chunkHeaders : Headers
  trailer : Trailers

abbrev Parser := Grammar.Data State

/-- Creates an initial empty state for parsing with a given URI parser -/
def State.empty : State :=
  { hasHost := false
  , chunkHeaders := Headers.empty
  , isChunked := false
  , req := Request.empty
  , res := Response.empty
  , contentLength := none
  , prop := Inhabited.default
  , value := Inhabited.default
  , trailer := Inhabited.default
  , uri := Uri.Parser.create }

/-- Processes a URL fragment and updates the URI in the state -/
private def onUrl (str: ByteArray) (data: State) : IO (State × Nat) := do
  let uri ← Uri.Parser.feed data.uri str
  pure ({ data with uri }, 0)

/-- Finalizes the URL parsing and updates the request's URI field -/
private def endUrl (data: State) : IO (State × Nat) := do
  match data.uri.data with
  | .ok uri => pure ({ data with uri := Uri.Parser.create, req := {data.req with uri}}, 0)
  | .error _ => pure (data, 1)

/-- Processes and finalizes a field (header) in the HTTP request -/
private def endField (data: State) : IO (State × Nat) := do
  let prop := data.prop.toLower
  let value := data.value

  let (data, code) :=
    match prop with
    | "host" =>
      if data.hasHost then
        (data, false)
      else
        let data := {data with hasHost := true }
        (data, (data.uri.info.authority.map (· != value)).getD true)
    | "transfer-encoding" =>
      let parts: Headers.Header .transferEncoding _ := inferInstance
      let parts := parts.parse value
      let parts := (Array.find? · Headers.TransferEncoding.isChunked) =<< parts
      if let some _ := parts
          then ({data with isChunked := true}, true)
          else (data, true)
    | _ => (data, true)

  let headers := data.req.headers.addRaw prop value
  pure ({ data with req := { data.req with headers }, prop := "", value := ""}, if code then 0 else 1)

private def onEndFieldExt (data: State) : IO (State × Nat) := do
  let prop := data.prop.toLower
  let value := data.value

  let chunkHeaders := data.chunkHeaders.addRaw prop value
  pure ({ data with chunkHeaders, prop := "", value := ""}, 0)

private def onEndFieldTrailer (data: State) : IO (State × Nat) := do
  let prop := data.prop.toLower
  let value := data.value

  let trailer := data.trailer.add prop value
  pure ({ data with trailer, prop := "", value := ""}, 0)

/-- Checks if the property being processed is "content-length" -/
private def endProp (data: State) : IO (State × Nat) := do
  pure (data, if data.prop.toLower == "content-length" then 1 else 0)

/-- Handles the body of the HTTP request and updates the request with the body content -/
private def onBody (fn: ByteArray → IO Unit) (body: ByteArray) (acc: State) : IO (State × Nat) := do
  fn body
  pure (acc, 0)

/-- Processes the request line to set the HTTP method and version in the state -/
private def onRequestLine (method: Nat) (major: Nat) (minor: Nat) (acc: State) : IO (State × Nat) := do
  let method := Option.get! $ Method.fromNumber method
  let version := Version.fromNumber major minor
  match version with
  | none => return (acc, 1)
  | some version => do
    let acc := {acc with req := {acc.req with version, method}}
    return (acc, 0)

/-- Processes the response line to set the HTTP method and version in the state -/
private def onResponseLine (statusCode: Nat) (major: Nat) (minor: Nat) (acc: State) : IO (State × Nat) := do
  -- TODO: Handle error
  let status := Option.get! $ Status.fromCode statusCode.toUInt16
  let version := Version.fromNumber major minor
  match version with
  | none => return (acc, 1)
  | some version => do
    let acc := {acc with res := {acc.res with version, status}}
    return (acc, 0)

/-- Finalizes the headers and sets the content length if present, checking for required conditions -/
private def onEndHeaders {isRequest: Bool} (callback: (if isRequest then Request else Response) → IO Unit) (content: Nat) (acc: State) : IO (State × Nat) := do
  let code :=
    if acc.isChunked then 1
    else if acc.hasHost then 0
    else 2

  match isRequest with
  | true => callback acc.req
  | false => callback acc.res

  pure ({acc with contentLength := some content}, code)

/-- Handles the body of the HTTP request and updates the request with the body content -/
private def onChunk (fn: Chunk → IO Unit) (body: ByteArray) (acc: State) : IO (State × Nat) := do
  fn (Chunk.mk acc.chunkHeaders body)
  pure ({acc with chunkHeaders := Inhabited.default}, 0)

/-- Handles the end of the request usually with a function that receives the trailer -/
private def onEndRequest (fn: Trailers → IO Unit) (acc: State) : IO (State × Nat) := do
  fn acc.trailer
  pure (State.empty, 0)

/-- Creates the HTTP request parser with the provided body callback -/
def Parser.create
    (isRequest: Bool)
    (endHeaders: (if isRequest then Request else Response) → IO Unit)
    (endBody: ByteArray → IO Unit)
    (endChunk: Chunk → IO Unit)
    (endTrailers: Trailers → IO Unit)
    : Parser
    :=
      let data :=
        Grammar.create
          (onReasonPhrase := toString (λ_ acc => pure (acc, 0)))
          (onProp := toString (λval acc => pure ({acc with prop := acc.prop.append val}, 0)))
          (onValue := toString (λval acc => pure ({acc with value := acc.value.append val}, 0)))
          (onUrl := toByteArray onUrl)
          (onBody := toByteArray (onBody endBody))
          (onChunkData := toByteArray (onChunk endChunk))
          (onEndHeaders := onEndHeaders endHeaders)
          (onEndProp := endProp)
          (onEndUrl := endUrl)
          (onEndField := endField)
          (onEndRequestLine := onRequestLine)
          (onEndFieldExt := onEndFieldExt)
          (onEndFieldTrailer := onEndFieldTrailer)
          (onEndRequest := onEndRequest endTrailers)
          (onEndResponseLine := onResponseLine)
          State.empty
      { data with type := if isRequest then 1 else 0 }
  where
    toByteArray func st en bt data := func (bt.extract st en) data
    toString func st en bt data := func (String.fromUTF8! $ bt.extract st en) data
    appendOr (data: Option String) (str: String) : Option String :=
      match data with
      | some res => some $ res.append str
      | none => some str

/-- Feeds data into the parser. This function takes a parser and a ByteArray, and processes the
data to update the parser state incrementally. -/
def Parser.feed (parser: Parser) (data: ByteArray) : IO Parser :=
  Grammar.parse parser data
