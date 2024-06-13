import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Body
import Http.Data.Uri
import Http.Classes

namespace Http.Data
open Http.Classes

/-- This module defines a HTTP [Request]. A request contains a bunch of parts like a method, uri,
headers and a [Incoming] body, that can be a stream. -/
structure Request where
  method  : Method
  uri     : Uri
  version : Version
  headers : Headers
  deriving Repr, Inhabited

namespace Request

def empty : Request :=
  Request.mk Method.get Uri.empty Version.v10 Inhabited.default

instance : Canonical .text Request where
  repr r :=
    let headerString := Canonical.text r.method ++ " " ++ Canonical.text r.uri ++ " " ++ Canonical.text r.version ++ "\r\n" ++ Canonical.text r.headers
    headerString ++ "\r\n\r\n"
