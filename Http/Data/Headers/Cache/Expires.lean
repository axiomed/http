import Http.Data.Headers.Name
import Http.Classes.Parseable
import Http.Util.Date
import Time

namespace Http.Data.Headers
open Http.Util.Date
open Http.Classes

/-- Gives the date/time that the response is considered expired

* Reference: https://httpwg.org/specs/rfc9111.html#field.expires
-/
def Expires := Time.DateTime .GMT

instance : Header .expires Expires where
  parse := Except.toOption ∘ Time.Format.choiceParse #[RFC822, RFC850, AscTime]
