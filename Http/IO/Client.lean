import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.IO.Server.Config
import Http.IO.Server.Connection
import Http.Data.Uri.Parser
import Http.Data
import Http.Util
import Http.Classes
import LibUV

namespace Http.IO.Client
open Http.Protocols.Http1.Data
open Http.Protocols.Http1
open Http.Data.Headers
open Http.Classes
open Http.Data

structure Client where
  socket: UV.TCP
  waiting: IO.Ref Bool

def request
  (host: String)
  (port: UInt16 := 80)
  (config: MessageConfig)
  (request: Request)
  (body: String)
  (onResponse : Response → IO Bool)
  (onData : Chunk → IO Unit)
  (onEnd : Trailers → IO Unit := λ_ => pure ())
  (onError : IO Unit := do pure ())
  : IO Unit := do
  let go : UV.IO Unit := do
    let loop ← UV.mkLoop
    let addr ← UV.SockAddr.mkIPv4 host port
    let client ← loop.mkTCP

    let _ ← client.connect addr λ_ => do
      let _ ← client.write #[ String.toUTF8 $ (Canonical.text request)
                            , String.toUTF8 body
                            ] (λ_ => pure ())

      let data := Parser.create config false
        (onResponse)
        (onData ∘ Chunk.mk Headers.empty)
        onData
        onEnd

      let ref ← IO.toUVIO (IO.mkRef data)

      client.read_start $ λr => do
        match r with
        | .eof => pure ()
        | .error _ => onError.toUVIO
        | .ok bytes => do
          let res ← IO.toUVIO $ Parser.feed config (← ref.get) bytes
          match res with
          | .ok res => ref.set res
          | .error s => do
            (IO.println s!"err: {repr s}").toUVIO
            onError.toUVIO
            client.stop

    let _ ← loop.run

  UV.IO.run go
