import Http.Data.Headers.Name
import Http.Classes.Parseable
import Http.Util.Date
import Time

namespace Http.Data.Headers
open Http.Util.Date
open Http.Classes

/-- The Date header represents the date and time at which the message was originated.

* Reference: https://httpwg.org/specs/rfc9110.html#field.date
-/
def Date := Time.DateTime .GMT

instance : Canonical .text (Time.DateTime .GMT) where
  repr date := RFC822.format date

instance : Header .date Date where
  parse := Except.toOption ∘ Time.Format.choiceParse #[RFC822, RFC850, AscTime]
