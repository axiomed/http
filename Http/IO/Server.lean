import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.Data.Uri.Parser
import Http.Data
import Http.IO.Server.Config
import Http.IO.Connection
import Http.Classes
import LibUV

namespace Http.IO.Server

/-! Definition of the common connection for HTTP protocols. -/

open Http.Protocols.Http1.Data
open Http.Protocols.Http1
open Http.Classes
open Http.Data

def IO.toUVIO (act: IO α) : UV.IO α := IO.toEIO (λx => UV.Error.user x.toString) act

def simpleStatusResponse (status: Status) (conn: Connection) := do
  conn.response.modify λres => res
    |>.withHeader "connection" "close"
    |>.withStatus status
  conn.end

def handleError (conn: Connection) : ParsingError → IO Unit
  | .invalidMessage => simpleStatusResponse .badRequest conn
  | .uriTooLong => simpleStatusResponse .uriTooLong conn
  | .bodyTooLong => simpleStatusResponse .payloadTooLarge conn
  | .headerTooLong => simpleStatusResponse .requestHeaderFieldsTooLarge conn
  | .headersTooLong => simpleStatusResponse .requestHeaderFieldsTooLarge conn

def onRequest (config: Config) (conn: IO.Ref Connection) (onReq: Connection → IO Unit) (request: Request) : IO Bool := do
  if let some (host, valid) := (·, ·) <$> request.headers.findRaw? "host" <*> config.host then
    if host ≠ valid then
      let conn ← conn.get
      handleError conn .invalidMessage
      return false

  if let some (.standard res) := request.headers.find? Headers.HeaderName.Standard.connection then
    let connRef ← conn.get
    match res with
    | .keepAlive => do
      connRef.isKeepAlive.set true
      connRef.withHeader "connection" "keep-alive"
    | .close => do
      connRef.isKeepAlive.set false
      connRef.withHeader "connection" "close"
    | .upgrade => pure ()

  conn.modify (λx => {x with request})
  let conn ← conn.get

  onReq conn

  let isClosing ← conn.isClosing.get

  return ¬isClosing

def closeConnectionTimeout (conn: IO.Ref Connection) : IO Unit := do
  let connection ← conn.get
  connection.close

def readSocket
  (loop: UV.Loop)
  (config: Config)
  (socket: UV.TCP)
  (onReq: Connection → IO Unit)
  (onData: Connection → Chunk → IO Unit)
  (onTrailer: Connection → Trailers → IO Unit)
  : UV.IO Unit
  := do
    let timer ← loop.mkTimer

    let onEnd := λ_ => do
      timer.stop
      socket.read_stop
      socket.stop

    let conn ← Connection.new socket (λ_ => UV.IO.run $ onEnd ())
    let connRef ← IO.toUVIO (IO.mkRef conn)

    let readRef : {x y: Type} → (Connection → y → IO x) → y → IO x := λfunc y => do
      let conn ← connRef.get
      func conn y

    let data := Parser.create config.messageConfig true
      (onRequest config connRef onReq)
      (readRef (onData · ∘ Chunk.mk Headers.empty))
      (readRef (onData ·))
      (readRef (onTrailer ·))

    let ref ← IO.toUVIO (IO.mkRef data)

    timer.start (config.idleTimeout.toUInt64 * 1000) 0 $ IO.toUVIO (closeConnectionTimeout connRef)

    socket.read_start fun
      | .eof => pure ()
      | .error _ => onEnd ()
      | .ok bytes => do
        let res ← IO.toUVIO $ Parser.feed config.messageConfig (← ref.get) bytes
        match res with
        | .ok res => ref.set res
        | .error err =>
            let conn ← connRef.get
            IO.toUVIO $ handleError conn err

def server
  (host: String)
  (port: UInt16)
  (config: Config := Inhabited.default)
  (onReq: Connection → IO Unit := (λ_ => pure ()))
  (onData: Connection → Chunk → IO Unit := (λ_ _ => pure ()))
  (onEnd: Connection → Trailers → IO Unit := (λ_ _ => pure ()))
  : IO Unit := do
  let go : UV.IO Unit := do
    let loop ← UV.mkLoop
    let addr ← UV.SockAddr.mkIPv4 host port
    let server ← loop.mkTCP
    server.bind addr
    server.listen 128 do
      let client ← loop.mkTCP
      server.accept client
      readSocket loop config client onReq onData onEnd
    let _ ← loop.run
  UV.IO.run go
