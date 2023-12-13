import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Uri

-- | A request is a message from the client to the server.
structure Request where
  method : Method
  uri: Uri
  version: Version
  headers : Headers
  body : String

instance : ToString Request where
  toString r :=
    let headerString := toString r.method ++ " " ++ toString r.uri ++ " " ++ toString r.version ++ "\r\n" ++ toString r.headers
    headerString ++ "\r\n\r\n" ++ r.body
