import Socket

import Http.Data.Version
import Http.Data.Request
import Http.Data.Response
import Http.Data.Headers
import Http.Data.Uri

open Socket


-- | A connection to a server.
structure Connection where
  socket  : Socket
  address : SockAddr

def Connection.send (connection: Connection) (request : Request) : IO (Except String Response) := do
  let _ ← connection.socket.send (String.toUTF8 $ ToString.toString request)
  let recv ← connection.socket.recv 4096
  match recv with
  | some recv => do
    let str := String.fromUTF8Unchecked recv
    let parsed := Grape.run Response.parser str.toSlice
    match parsed with
    | Result.done response _ => pure $ Except.ok response
    | Result.error err _     => pure $ Except.error (toString err)
    | Result.cont _          => pure $ Except.error "Incomplete response"
  | none => pure $ Except.error "Invalid UTF-8"

def Connection.create (address: String) (port: String) : IO Connection := do
  let socket ← Socket.mk AddressFamily.inet SockType.stream
  let localAddr ← SockAddr.mk address port AddressFamily.inet SockType.stream
  Socket.setBlocking socket true
  let _ ← socket.connect localAddr
  pure { socket, address := localAddr }

def Connection.post (connection: Connection) (uri: Uri) (body: String) : IO (Except String Response) := do
  let request := Request.mk Method.post uri Version.v10
    (Headers.empty
    |> Headers.with "Content-Type" "application/json"
    |> Headers.with "Content-Length" (toString body.length))
    body
  connection.send request

def Connection.get (connection: Connection) (uri : Uri) : IO (Except String Response) := do
  let request := Request.mk Method.get uri Version.v10 Headers.empty ""
  connection.send request
