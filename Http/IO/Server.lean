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
  conn.end false

def handleError (conn: Connection) : ParsingError → IO Unit
  | .invalidMessage => simpleStatusResponse .badRequest conn
  | .uriTooLong => simpleStatusResponse .uriTooLong conn
  | .bodyTooLong => simpleStatusResponse .payloadTooLarge conn
  | .headerTooLong => simpleStatusResponse .requestHeaderFieldsTooLarge conn
  | .headersTooLong => simpleStatusResponse .requestHeaderFieldsTooLarge conn

def onConnection (conn: IO.Ref Connection) (onConn: Connection → IO Bool) (request: Request) : IO Bool := do
  conn.modify (λx => {x with request})
  let conn ← conn.get
  onConn conn

def closeConnectionTimeout (conn: IO.Ref Connection) : IO Unit := do
  let connection ← conn.get
  let isClosing ← connection.isClosing.get
  if ¬isClosing then connection.close

def readSocket
  (loop: UV.Loop)
  (config: Config)
  (socket: UV.TCP)
  (onConn: Connection → IO Bool)
  (onData: Connection → Chunk → IO Unit)
  (onTrailer: Connection → Trailers → IO Unit)
  : UV.IO Unit
  := do
    let conn ← Connection.new socket
    let connRef ← IO.toUVIO (IO.mkRef conn)

    let readRef : {x y: Type} → (Connection → y → IO x) → y → IO x := λfunc y => do
      let conn ← connRef.get
      func conn y

    let data := Parser.create config.messageConfig true
      (onConnection connRef onConn)
      (readRef (onData · ∘ Chunk.mk Headers.empty))
      (readRef (onData ·))
      (readRef (onTrailer ·))

    let timer ← loop.mkTimer

    let ref ← IO.toUVIO (IO.mkRef data)

    socket.read_start fun
      | .eof => IO.toUVIO $ closeConnectionTimeout connRef
      | .error _ => socket.read_stop *> socket.stop
      | .ok bytes => do
        timer.stop
        timer.start (config.idleTimeout.toUInt64 * 1000) 0 $ IO.toUVIO (closeConnectionTimeout connRef)
        let res ← IO.toUVIO $ Parser.feed config (← ref.get) bytes
        match res with
        | .ok res => ref.set res
        | .error err =>
            let conn ← connRef.get
            IO.toUVIO $ handleError conn err

def server
  (config: Config)
  (host: String) (port: UInt16)
  (onConn: Connection → IO Bool)
  (onData: Connection → Chunk → IO Unit)
  (onTrailer: Connection → Trailers → IO Unit)
  : IO Unit := do
  let go : UV.IO Unit := do
    let loop ← UV.mkLoop
    let addr ← UV.SockAddr.mkIPv4 host port
    let server ← loop.mkTCP
    server.bind addr
    server.listen 128 do
      let client ← loop.mkTCP
      server.accept client
      readSocket loop config client onConn onData onTrailer
    let _ ← loop.run
  UV.IO.run go
