import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Status

-- | A request is a message from the client to the server.
structure Response where
  version      : Version
  status       : Status
  reasonPhrase : String
  headers      : Headers
  body         : String

instance : ToString Response where
  toString r :=
    let headerString := toString r.status.toCode ++ " " ++ toString r.version ++ "\r\n" ++ toString r.headers
    headerString ++ "\r\n\r\n" ++ r.body

def Response.parser : Grape.Grape Response := do
  Grape.string "HTTP/"
  let version ← Version.parser
  Grape.string " "
  let statusCode ← Grape.Text.number
  Grape.string " "
  let reasonPhrase ← Grape.takeWhile (fun c => c ≠ 13)
  Grape.string "\r\n"
  let headers ← Headers.parser
  Grape.string "\r\n"
  let statusCode := Status.fromCode statusCode
  let body ← Grape.takeUntilEnd
  let body := body.toASCIIString

  match statusCode with
  | Option.none        => Grape.fail "Invalid status code"
  | Option.some status => Grape.pure { version, status, reasonPhrase := reasonPhrase.toASCIIString, headers, body }
