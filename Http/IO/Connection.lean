import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.IO.Buffer
import Http.Data.Uri.Parser
import Http.Data
import LibUV

namespace Http.IO

open Http.Protocols.Http1.Data
open Http.Data.Headers
open Http.Data

structure Connection where
  isClosing: IO.Ref Bool
  isChunked: Bool
  isHead: Bool
  request: Data.Request
  response: IO.Ref Data.Response
  buffer: IO.Ref Buffer
  socket: UV.TCP

def Connection.new (socket: UV.TCP) : UV.IO Connection := do
  let isClosing ← IO.mkRef false
  let buffer ← IO.mkRef #[ByteArray.empty]
  let response ← IO.mkRef Data.Response.empty
  return { isClosing, isHead := false, isChunked := true, request := Data.Request.empty, socket, buffer, response }

def Connection.guard (connection: Connection) (func: IO Unit) : IO Unit := do
  let isClosing ← connection.isClosing.get
  if ¬isClosing then func

def Connection.close (connection: Connection) : IO Unit := connection.guard do
  UV.IO.run connection.socket.stop
  connection.isClosing.set true

def Connection.write (connection: Connection) (data: Chunk) : IO Unit := connection.guard do
  connection.buffer.modify (ToBuffer.toBuffer · data)

def Connection.rawWrite (connection: Connection) (buffer: Buffer) : IO Unit := do
  UV.IO.run do let _ ← connection.socket.write buffer (λ_ => pure ())

def Connection.flushBody (connection: Connection) : IO Unit := connection.guard do
  unless connection.isHead do
    let buffer ← connection.buffer.swap #[]
    connection.rawWrite buffer

def Connection.end (connection: Connection) (alive: Bool) : IO Unit := connection.guard do
  let response ← connection.response.get

  connection.rawWrite (ToBuffer.toBuffer #[] response)
  connection.flushBody

  if let some res := response.headers.find? HeaderName.Standard.transferEncoding then
    let res := res.find? Headers.TransferEncoding.isChunked
    if res.isSome then
      connection.rawWrite (ToBuffer.toBuffer #[] Chunk.zeroed)

  if !alive then
    connection.close
