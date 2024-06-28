import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.IO.Server.Config
import Http.IO.Server.Connection
import Http.Data.Uri.Parser
import Http.Data
import Http.Util
import Http.Classes
import LibUV

namespace Http.IO.Server
open Http.Protocols.Http1.Data
open Http.Protocols.Http1
open Http.Data.Headers
open Http.Classes
open Http.Data

def simpleStatusResponse (status: Status) (conn: Connection) := do
  conn.response.modify λres => res
    |>.withHeaderStd .connection ConnectionHeader.close
    |>.withStatus status
  conn.end

def handleError (conn: Connection) : ParsingError → IO Unit
  | .invalidMessage _ => simpleStatusResponse .badRequest conn
  | .uriTooLong => simpleStatusResponse .uriTooLong conn
  | .bodyTooLong => simpleStatusResponse .payloadTooLarge conn
  | .headerTooLong => simpleStatusResponse .requestHeaderFieldsTooLarge conn
  | .headersTooLong => simpleStatusResponse .requestHeaderFieldsTooLarge conn

def keepAlive (config: Config) : KeepAlive :=
  KeepAlive.new (some $ config.idleTimeout / 1000) config.maxKeepAliveRequests

def onRequest (config: Config) (connRef: IO.Ref Connection) (onReq: Connection → IO Unit) (request: Request) : IO Bool := do
  let conn ← connRef.get
  conn.requests.modify (· + 1)

  if let some (host, valid) := (·, ·) <$> request.headers.findRaw? "host" <*> config.host then
    if host ≠ valid then
      return false

  if let some max := config.maxKeepAliveRequests then
    let counter ← conn.requests.get
    if counter > max then
      return false

  if let some (.standard res) := request.headers.find? HeaderName.Standard.connection then
    match res with
    | .keepAlive => do
      conn.isKeepAlive.set true
      conn.withHeaderStd .connection .keepAlive
      conn.withHeaderStd .keepAlive (keepAlive config)
    | .close => do
      conn.isKeepAlive.set false
      conn.withHeaderStd .connection .close
    | .upgrade => pure ()

  connRef.modify (λx => {x with request})
  let conn ← connRef.get

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

    let timerCount := do
      timer.start config.idleTimeout.toUInt64 0 $ do
        IO.toUVIO (closeConnectionTimeout connRef)

    timerCount

    socket.read_start fun
      | .eof => pure ()
      | .error _ => onEnd ()
      | .ok bytes => do
        timer.stop
        let res ← IO.toUVIO $ Parser.feed config.messageConfig (← ref.get) bytes
        timerCount
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
