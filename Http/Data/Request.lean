import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Uri


-- | A request is a message from the client to the server.
structure Request where
  method  : Method
  uri     : Uri
  version : Version
  headers : Headers
  body    : String

instance : ToString Request where
  toString r :=
    let headerString := toString r.method ++ " " ++ toString r.uri ++ " " ++ toString r.version ++ "\r\n" ++ toString r.headers
    headerString ++ "\r\n\r\n" ++ r.body

def Request.empty : Request :=
  Request.mk Method.get Uri.empty Version.v10 Headers.empty ""

def Request.withHeader (request : Request) (name : String) (value : String) : Request :=
  { request with headers := request.headers.with name value }

def Request.withBody (request : Request) (body : String) : Request :=
  -- Technically, some other methods should also not allow body,
  -- but it's a higher chance it happens by mistake with default GET
  match request.method with
  | Method.get => { request with method := Method.post, body }
  | _ => { request with body }

