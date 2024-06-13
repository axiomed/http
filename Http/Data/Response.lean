import Http.Data.Headers
import Http.Data.Version
import Http.Data.Method
import Http.Data.Status
import Http.Data.Body
import Http.Classes

namespace Http.Data
open Http.Classes

/-! HTTP [Response] with a bunch of parts like version and status and a body with the α type
that can be anything that can be transformed into a byte sequence -/
structure Response where
  status       : Status
  version      : Version
  headers      : Headers

namespace Response

def withHeaderStd (res: Response) [Canonical .text α] (name: Headers.HeaderName.Standard) (value: α) [Headers.Header name α] : Response :=
  {res with headers := res.headers.addRaw name (Canonical.text value)}

def withHeader (res: Response) (name: String) (value: String) : Response :=
  {res with headers := res.headers.addRaw name value}

def withStatus (res: Response) (status: Status) : Response :=
  {res with status}

def empty : Response :=
  Response.mk Status.ok Version.v11 Inhabited.default

instance : Canonical .text Response where
  repr r :=
    let headerString := Canonical.text r.version ++ " " ++ toString r.status.toCode ++ " " ++ Canonical.text r.status ++ "\r\n" ++ Canonical.text r.headers
    headerString ++ "\r\n\r\n"
