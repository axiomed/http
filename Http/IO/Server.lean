import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.Data.Uri.Parser
import Http.Data
import Http.IO.Buffer
import Http.IO.Server.Config
import Http.IO.Connection
import LibUV

namespace Http.IO.Server

/-! Definition of the common connection for HTTP protocols. -/

open Http.Protocols.Http1.Data
open Http.Protocols.Http1
open Http.Data

def IO.toUVIO (act: IO α) : UV.IO α := IO.toEIO (λx => UV.Error.user x.toString) act

def onConnection (conn: IO.Ref Connection) (onConn: Connection → IO Unit) (request: Request) : IO Unit := do
  conn.modify (λx => {x with request})
  let conn ← conn.get
  onConn conn

def badRequest (socket: UV.TCP) (on_error: UV.IO Unit) := do
  let headers := Headers.empty
              |>.add "Connection" "close"
  let response := Response.mk (Version.v11) (Status.badRequest) "Bad Request" headers
  let _ ← socket.write #[(ToString.toString response).toUTF8] (λ_ => pure ())
  on_error

def readSocket
  (socket: UV.TCP) (on_error: UV.IO Unit)
  (onConn: Connection → IO Unit)
  (onData: Connection → Chunk → IO Unit)
  (onTrailer: Connection → Trailers → IO Unit)
  : UV.IO Unit
  := do
    let conn ← Connection.new socket
    let connRef ← IO.toUVIO (IO.mkRef conn)

    let readRef : {x y: Type} → (Connection → y → IO x) → y → IO x := λfunc y => do
      let conn ← connRef.get
      func conn y

    let data := Parser.create true
      (onConnection connRef onConn)
      (readRef (onData · ∘ Chunk.mk Headers.empty))
      (readRef (onData ·))
      (readRef (onTrailer ·))

    let ref ← IO.toUVIO (IO.mkRef data)
    socket.read_start fun
      | .eof => pure ()
      | .error _ => do
        socket.read_stop
        socket.stop
      | .ok bytes => do
        let data ← ref.get
        let res ← IO.toUVIO $ Parser.feed data bytes
        ref.set res
        IO.toUVIO $ IO.println s!"--- {res.error} {repr res.state}"
        if res.error ≠ 0 then
          badRequest socket on_error

def server (host: String) (port: UInt16)
  (onConn: Connection → IO Unit)
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
      let onEOF := do
        client.read_stop
        client.stop
      readSocket client onEOF onConn onData onTrailer
    let _ ← loop.run
  UV.IO.run go
