import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.Data.Uri.Parser
import Http.Classes
import Http.Data
import LibUV

namespace Http.IO
open Http.Protocols.Http1.Data
open Http.Data.Headers
open Http.Data
open Http.Classes

structure Connection where
  isClosing: IO.Ref Bool
  isChunked: Bool
  isHead: Bool
  request: Data.Request
  response: IO.Ref Data.Response
  buffer: IO.Ref (Array ByteArray)
  socket: UV.TCP

def Connection.new (socket: UV.TCP) : UV.IO Connection := do
  let isClosing ← IO.mkRef false
  let buffer ← IO.mkRef #[ByteArray.empty]
  let response ← IO.mkRef Data.Response.empty
  return { isClosing, isHead := false, isChunked := false, request := Data.Request.empty, socket, buffer, response }

def Connection.guard (connection: Connection) (func: IO Unit) : IO Unit := do
  let isClosing ← connection.isClosing.get
  if ¬isClosing then func

def Connection.close (connection: Connection) : IO Unit := connection.guard do
  UV.IO.run connection.socket.stop
  connection.isClosing.set true

def Connection.writeStr (connection: Connection) (data: String) : IO Unit := connection.guard do
  if connection.isChunked
    then connection.buffer.modify (λx => x.push $ Canonical.binary (Chunk.fromString data))
    else connection.buffer.modify (λx => x.push $ String.toUTF8 data)

def Connection.rawWrite (connection: Connection) (buffer: Array ByteArray) : IO Unit := do
  UV.IO.run do let _ ← connection.socket.write buffer (λ_ => pure ())

def Connection.flushBody (connection: Connection) : IO Unit := connection.guard do
  unless connection.isHead do
    let buffer ← connection.buffer.swap #[]
    connection.rawWrite buffer

def Connection.end (connection: Connection) (alive: Bool) : IO Unit := connection.guard do
  let response ← connection.response.get

  connection.rawWrite #[String.toUTF8 $ Canonical.text response]
  connection.flushBody

  if connection.isChunked then
      connection.rawWrite #[Canonical.binary Chunk.zeroed]

  connection.response.set Data.Response.empty

  if !alive then
    connection.close
