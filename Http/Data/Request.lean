import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Body
import Http.Data.Uri

namespace Http.Data

/-- This module defines a HTTP [Request]. A request contains a bunch of parts like a method, uri,
headers and a [Incoming] body, that can be a stream. -/
structure Request where
  method  : Method
  uri     : Uri
  version : Version
  headers : Headers

namespace Request

def empty : Request :=
  Request.mk Method.get Uri.empty Version.v10 Inhabited.default
