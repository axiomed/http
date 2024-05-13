import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Uri

namespace Http.Data

set_option linter.all true

/-- A request is a message from the client to the server. -/
structure Request where
  /-- Method specifies the HTTP Method -/
  method  : Method
  /-- Specifies the URI being requested -/
  uri     : Uri
  /-- The version of the protocol that is being used in this message -/
  version : Version
  /-- A map containing all the fields received -/
  headers : Headers
  deriving Repr

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
