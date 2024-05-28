import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Uri

namespace Http.Data

/-! This module defines a HTTP [Request]. A request contains a bunch of parts like a method, uri,
    headers and a [Incoming] body, that can be a stream.
-/

/-- A request is a message from the client to the server. -/
structure Request where
  method : Method
  uri : Uri
  version : Version
  headers : Headers
  body : String

def Request.empty :=
  Request.mk Method.get Uri.empty Version.v10 Inhabited.default ""
