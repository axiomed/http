import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Status

namespace Http.Data

-- | A request is a message from the client to the server.
structure Response where
  version      : Version
  status       : Status
  reasonPhrase : Substring
  headers      : Headers
  body         : String

instance : ToString Response where
  toString r :=
    let headerString := toString r.status.toCode ++ " " ++ toString r.version ++ "\r\n" ++ toString r.headers
    headerString ++ "\r\n\r\n" ++ r.body
