import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Status

import Http.IO.Buffer

namespace Http.Data

open Http.IO

/-! HTTP [Response] with a bunch of parts like version and status and a body with the [Outcome] type
that can be anything that can be transformed into a String
-/

/-- A request is a message from the client to the server. -/
structure Response where
  version : Version
  status : Status
  reasonPhrase : String
  headers : Headers

def Response.empty :=
  Response.mk Version.v11 Status.ok Status.ok.text Inhabited.default

def Response.setStatus (res: Response) (status: Status) : Response :=
  { res with status, reasonPhrase := status.text }

instance : ToString Response where
  toString r :=
    let headerString := toString r.version ++ " " ++ toString r.status.toCode ++ " " ++ r.reasonPhrase ++ "\r\n" ++ toString r.headers
    headerString ++ "\r\n\r\n"

instance : Serialize Response where
  serialize res := BufferBuilder.write (toString res)
