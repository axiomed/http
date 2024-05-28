import Http.Data
import Http.Data.Uri.Parser
import Http.Parser
import LibUV

namespace Http.IO.Connection

open Http.Data

def IO.toUVIO (act: IO α) : UV.IO α := IO.toEIO (λx => UV.Error.user x.toString) act

structure Accumulate where
  contentLength : Option Nat
  prop : String
  value : String
  req: Request
  hasHost : Bool
  isChunked : Bool
  uri: Uri.Parser.Data Http.Data.Uri
  body : String

structure Connection where
  version : Version

def Accumulate.empty (uri: Uri.Parser.Data Http.Data.Uri) : Accumulate :=
  { req := Request.empty
  , contentLength := none
  , isChunked := false
  , prop := ""
  , value := ""
  , hasHost := false
  , uri
  , body := "" }

def toStr (func: String → α → IO (α × Nat)) (st: Nat) (en: Nat) (bt: ByteArray) (data: α) : IO (α × Nat) :=
  func (String.fromUTF8! (bt.extract st en)) data

def toBS (func: ByteArray → α → IO (α × Nat)) (st: Nat) (en: Nat) (bt: ByteArray) (data: α) : IO (α × Nat) :=
  func (bt.extract st en) data

def noop : Nat -> Nat -> ByteArray -> α -> IO (α × Nat) := λ_ _ _ a => pure (a, 0)

def appendOr (data: Option String) (str: String) : Option String :=
  match data with
  | some res => some $ res.append str
  | none => some str

def uriParser : (Uri.Parser.Data Http.Data.Uri) :=
  Uri.Parser.create
    (onPath := toStr (λval acc => pure ({acc with path := appendOr acc.path val }, 0)))
    (onPort := toStr (λval acc => pure ({acc with port := appendOr acc.port val }, 0)))
    (onSchema := toStr (λval acc => pure ({acc with scheme := appendOr acc.scheme val }, 0)))
    (onHost := toStr (λval acc => pure ({acc with authority := appendOr acc.authority val }, 0)))
    (onQuery := toStr (λval acc => pure ({acc with query := appendOr acc.query val }, 0)))
    (onFragment := toStr (λval acc => pure ({acc with fragment := appendOr acc.fragment val }, 0)))
    Inhabited.default

def onUrl (str: ByteArray) (data: Accumulate) : IO (Accumulate × Nat) := do
  let uri ← Uri.Parser.parse data.uri str
  pure ({ data with uri }, 0)

def endUrl (data: Accumulate) : IO (Accumulate × Nat) := do
  let uriParser := uriParser
  let uri := data.uri.info
  pure ({ data with uri := uriParser, req := {data.req with uri } }, 0)

def endField (data: Accumulate) : IO (Accumulate × Nat) := do
  let prop := data.prop.toLower
  let value := data.value

  let (data, code) :=
    match prop with
    | "host" =>
      if !data.hasHost then
        let data := {data with hasHost := true }
        (data, (data.uri.info.authority.map (· != value)).getD true)
      else
        (data, false)
    | "transfer-encoding" =>
      let parts := value.split (· == '\n')
                |>.map String.trim
      if parts.contains "chunked"
          then ({data with isChunked := true}, true)
          else (data, true)
    | _ => (data, true)

  let req :=  {data.req with headers := data.req.headers.add prop value}
  pure ({ data with req, prop := "", value := ""}, if code then 0 else 1)

def endProp (data: Accumulate) : IO (Accumulate × Nat) := do
  pure (data, if data.prop.toLower == "content-length" then 1 else 0)

def onBody (fn: Request → IO Unit) (body: String) (acc: Accumulate) : IO (Accumulate × Nat) := do
  fn {acc.req with body};
  pure (acc, 0)

def onRequestLine (method: Nat) (major: Nat) (minor: Nat) (acc: Accumulate) : IO (Accumulate × Nat) := do
  let acc := {acc with req := {acc.req with version := Version.mk major minor, method := (Method.fromNumber method).get!}}
  return (acc, 0)

def onEndHeaders (content: Nat) (acc: Accumulate) : IO (Accumulate × Nat) := do
  pure ( {acc with contentLength := some content }
       , if acc.isChunked then 1
         else if acc.hasHost then 0
         else 2)

def requestParser (fn: Request → IO Unit) : Parser.Data Accumulate :=
    Parser.create
      (onReasonPhrase := noop)
      (onUrl := toBS onUrl)
      (onBody := toStr (onBody fn))
      (onProp := toStr (λval acc => pure ({acc with prop := acc.prop.append val}, 0)))
      (onValue := toStr (λval acc => pure ({acc with value := acc.value.append val}, 0)))
      (onEndProp := endProp)
      (onEndUrl := endUrl)
      (onEndField := endField)
      (onEndRequestLine := onRequestLine)
      (onEndHeaders := onEndHeaders)
      (onChunkData := toStr $ λstr acc => dbg_trace s!"chunk: '{str}'"; pure (acc, 0))
      (onChunkExtensionVal := toStr $ λstr acc => dbg_trace s!"val: '{str}'"; pure (acc, 0))
      (onChunkExtensionName := toStr $ λstr acc => dbg_trace s!"name: '{str}'"; pure (acc, 0))
      (Accumulate.empty (uriParser))

def parse (data: Parser.Data Accumulate) (arr: ByteArray) : UV.IO (Parser.Data Accumulate) :=
   IO.toUVIO $ Parser.parse data arr

-- Socket

def readSocket (socket: UV.TCP) (on_error: UV.IO Unit) (fn: Request → IO Response) : UV.IO Unit := do
  let data := requestParser $ λreq => UV.IO.run $ do
    let resp ← IO.toUVIO $ fn req
    let _ ← socket.write #[(ToString.toString resp).toUTF8] (λ_ => pure ())

  let ref ← IO.toUVIO (IO.mkRef data)

  socket.read_start fun
    | .error _ => do
      socket.read_stop
      socket.stop
    | .eof => pure ()
    | .ok bytes => do
      let data ← ref.get
      let res ← parse data bytes
      ref.set res
      if res.error == 1 then
        let response := Response.mk (Version.v11) (Status.badRequest) "Bad Request" Headers.empty ""
        let _ ← socket.write #[(ToString.toString response).toUTF8] (λ_ => pure ())
        on_error

def server (host: String) (port: UInt16) (fn: Request → IO Response) (timeout : UInt64 := 10000) : IO Unit := do
  let go : UV.IO Unit := do
    let loop ← UV.mkLoop
    let addr ← UV.SockAddr.mkIPv4 host port
    let server ← loop.mkTCP
    server.bind addr
    server.listen 128 do
      let client ← loop.mkTCP
      server.accept client
      let onEOF := do
        client.read_stop
        client.stop
      readSocket client onEOF fn
    let still ← loop.run
    UV.log s!"res {still}"
  UV.IO.run go
