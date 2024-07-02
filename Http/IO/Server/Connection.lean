import Http.Protocols.Http1.Parser
import Http.Protocols.Http1.Data
import Http.Data.URI.Parser
import Http.Classes
import Http.Data
import LibUV

namespace Http.IO.Server
open Http.Protocols.Http1.Data
open Http.Data.Headers
open Http.Data
open Http.Classes

structure Connection (α: Type) where
  isClosing: IO.Ref Bool
  sentHeaders: IO.Ref Bool
  isChunked: IO.Ref Bool
  isHead: IO.Ref Bool
  isKeepAlive: IO.Ref Bool
  requests: IO.Ref Nat

  request: Data.Request
  response: IO.Ref Data.Response
  buffer: IO.Ref (Array ByteArray)
  size: IO.Ref Nat
  socket: UV.TCP
  onEnd: Unit → IO Unit

  data: IO.Ref α

def Connection.new (socket: UV.TCP) (onEnd: Unit → IO Unit) (data: α) : UV.IO (Connection α) := do
  return { isClosing := ← IO.mkRef false
         , isHead := ← IO.mkRef false
         , requests := ← IO.mkRef 0
         , isChunked := ← IO.mkRef false
         , isKeepAlive := ← IO.mkRef false
         , sentHeaders := ← IO.mkRef false
         , size := ← IO.mkRef 0
         , request := Data.Request.empty
         , buffer := ← IO.mkRef #[ByteArray.empty]
         , response := ← IO.mkRef Data.Response.empty
         , data := ← IO.mkRef data
         , socket
         , onEnd }

def Connection.guard (connection: Connection α) (func: IO Unit) : IO Unit := do
  let isClosing ← connection.isClosing.get
  if ¬isClosing then func

def Connection.withHeader (connection: Connection α) (name: String) (value: String) : IO Unit := connection.guard do
  let sentHeaders ← connection.sentHeaders.get
  if sentHeaders then
    throw (IO.userError (toString "cannot write headers after they were sent"))
  connection.response.modify (·.withHeader name value)

def Connection.setStatus (connection: Connection α) (status: Status) : IO Unit := connection.guard do
  connection.response.modify ({· with status := status})

def Connection.withHeaderStd (connection: Connection β) [Canonical .text α] (name: Headers.HeaderName.Standard) (value: α) [Headers.Header name α] : IO Unit := connection.guard do
  let sentHeaders ← connection.sentHeaders.get
  if sentHeaders then
    throw (IO.userError (toString "cannot write headers after they were sent"))
  connection.response.modify (·.withHeaderStd name value)

def Connection.close (connection: Connection α) : IO Unit := connection.guard do
  connection.isClosing.set true
  connection.onEnd ()

def Connection.writeByteArray (connection: Connection α) (data: ByteArray) : IO Unit := connection.guard do
  let isChunked ← connection.isChunked.get
  let data :=
    if isChunked
      then Canonical.binary (Chunk.mk Inhabited.default data)
      else data
  connection.size.modify (· + data.size)
  connection.buffer.modify (λx => x.push data)

def Connection.write (connection: Connection α) (data: String) : IO Unit := connection.guard do
  connection.writeByteArray (String.toUTF8 data)

def Connection.rawWrite (connection: Connection α) (buffer: Array ByteArray) : IO Unit := connection.guard do
  if buffer.size > 0 then
    UV.IO.run do let _ ← connection.socket.write buffer (λ_ => pure ())

def Connection.sendLastChunk (connection: Connection α) : IO Unit := connection.guard do
  let isChunked ← connection.isChunked.get
  if isChunked then
    connection.rawWrite #[Canonical.binary Chunk.zeroed]

def Connection.sendHeaders (connection: Connection α) : IO Unit := connection.guard do
  let sent ← connection.sentHeaders.get

  if ¬sent then
    let response ← connection.response.get

    if let some encoding := response.headers.find? .transferEncoding then
      if encoding.find? Headers.TransferEncoding.isChunked |>.isSome then
        connection.isChunked.set true
    else do
      let res ← connection.size.swap 0
      connection.withHeader "Content-Length" (toString res)

    let response ← connection.response.get

    connection.rawWrite #[String.toUTF8 $ Canonical.text response]
    connection.sentHeaders.set true

def Connection.flush (connection: Connection α) : IO Unit := connection.guard do
  connection.sendHeaders
  let buffer ← connection.buffer.swap #[]
  connection.rawWrite buffer

def Connection.end (connection: Connection α) : IO Unit := connection.guard do
  connection.flush
  connection.sendLastChunk

  connection.response.set Data.Response.empty

  let isKeepAlive ← connection.isKeepAlive.get

  if ¬isKeepAlive then
    connection.close

  connection.sentHeaders.set false
  connection.isChunked.set false
  connection.isHead.set false
  connection.isKeepAlive.set false
