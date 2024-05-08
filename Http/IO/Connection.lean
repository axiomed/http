import Http.Data
import Http.Parser
import Http.URI.Parser
import LibUV

namespace Http.IO.Connection

open Http.Data

-- ASYNC IO Component

abbrev AIO := StateT UV.Loop UV.IO

def AIO.loop : AIO UV.Loop :=
  StateT.get

def IO.toUVIO (act: IO α) : UV.IO α := IO.toEIO (λx => UV.Error.user x.toString) act

-- Parsed

structure AccURI where
  uri : Http.Data.Uri
  deriving Inhabited, Repr

structure Accumulate where
  contentLength : Option UInt64
  prop : String
  value : String
  req: Request
  uri: URI.Parser.Data AccURI

def Accumulate.empty (uri: URI.Parser.Data AccURI) : Accumulate :=
  { req := Request.empty
  , contentLength := none
  , prop := ""
  , value := ""
  , uri }

def toStr (func: String → α → IO (α × Int)) (st: Nat) (en: Nat) (bt: ByteArray) (data: α) : IO (α × Int) :=
  func (String.fromUTF8! (bt.extract st en)) data

def toBS (func: ByteArray → α → IO (α × Int)) (st: Nat) (en: Nat) (bt: ByteArray) (data: α) : IO (α × Int) :=
  func (bt.extract st en) data

def noop : Nat -> Nat -> ByteArray -> α -> IO (α × Int) := λ_ _ _ a => pure (a, 0)

def appendOr (data: Option String) (str: String) : Option String :=
  match data with
  | some res => some $ res.append str
  | none => some str

def uriParser : UV.IO (URI.Parser.Data AccURI) := do
  IO.toUVIO $
    URI.Parser.create
      (on_path := toStr (λval acc => pure ({acc with uri := {acc.uri with path := appendOr acc.uri.path val }}, 0)))
      (on_port := toStr (λval acc => pure ({acc with uri := {acc.uri with port := appendOr acc.uri.port val }}, 0)))
      (on_schema := toStr (λval acc => pure ({acc with uri := {acc.uri with scheme := appendOr acc.uri.scheme val }}, 0)))
      (on_host := toStr (λval acc => pure ({acc with uri := {acc.uri with authority := appendOr acc.uri.authority val }}, 0)))
      (on_query := toStr (λval acc => pure ({acc with uri := {acc.uri with query := appendOr acc.uri.query val }}, 0)))
      (on_fragment := toStr (λval acc => pure ({acc with uri := {acc.uri with fragment := appendOr acc.uri.fragment val }}, 0)))
      Inhabited.default

def on_url (str: ByteArray) (data: Accumulate) : IO (Accumulate × Int) := do
  let uri ← URI.Parser.parse data.uri str
  pure ({ data with uri }, 0)

def end_url (data: Accumulate) : IO (Accumulate × Int) := do
  let uriParser ← UV.IO.run uriParser
  let uri := data.uri.data.uri
  pure ({ data with uri := uriParser, req := {data.req with uri } }, 0)

def end_field (data: Accumulate) : IO (Accumulate × Int) := do
  pure ({ data with req := {data.req with headers := data.req.headers.add data.prop data.value }, prop := "", value := ""}, 0)

def end_prop (data: Accumulate) : IO (Accumulate × Int) := do
  pure (data, if data.prop.toLower == "content-length" then 1 else 0)

def on_body (fn: Request → IO Unit) (body: String) (acc: Accumulate) : IO (Accumulate × Int) := do

  fn {acc.req with body};
  pure (acc, 0)

def requestParser (fn: Request → IO Unit) : UV.IO (Parser.Data Accumulate) := do
  IO.toUVIO $
    Parser.create
      (on_reasonPhrase := noop)
      (on_url := toBS on_url)
      (on_body := toStr (on_body fn))
      (on_prop := toStr (λval acc => pure ({acc with prop := acc.prop.append val}, 0)))
      (on_value := toStr (λval acc => pure ({acc with value := acc.value.append val}, 0)))
      (on_endProp := end_prop)
      (on_endUrl := end_url)
      (on_endField := end_field)
      (on_endRequestLine := λacc method major minor => pure ({acc with req := {acc.req with version := Version.mk major minor, method := (Method.fromNumber method).get!}}, 0))
      (Accumulate.empty (← uriParser))

def parse (data: Parser.Data Accumulate) (arr: ByteArray) : UV.IO (Parser.Data Accumulate) :=
   IO.toUVIO $ Parser.parse data arr

-- Socket

def readSocket (socket: UV.TCP) (on_eof: UV.IO Unit) (fn: Request → UV.IO Response) : UV.IO Unit := do
  let data ← requestParser $ λreq => UV.IO.run $ do
    let resp ← fn req
    let _ ← socket.write #[(ToString.toString resp).toUTF8] (λ_ => pure ())

  let ref ← IO.toUVIO (IO.mkRef data)
  socket.read_start fun
    | .error _ => do
      socket.read_stop
      socket.stop
    | .eof => do
      on_eof
    | .ok bytes => do
      let data ← ref.get
      let res ← parse data bytes
      ref.set res

def server (host: String) (port: UInt16) (fn: Request → UV.IO Response) (timeout : UInt64 := 10000) : IO Unit := do
  let go : UV.IO Unit := do
    let loop ← UV.mkLoop
    let addr ← UV.SockAddr.mkIPv4 host port
    let server ← loop.mkTCP
    server.bind addr
    server.listen 128 do
      let client ← loop.mkTCP
      server.accept client
      let timer ← loop.mkTimer
      let onEOF := do timer.stop
      timer.start timeout (timeout * 2) do onEOF
      readSocket client onEOF fn
    let _ ← loop.run
  UV.IO.run go
