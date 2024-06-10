import Http.Data.Headers.Name
import Http.Util.Date
import Time

namespace Http.Data.Headers
open Http.Util.Date

instance : Header .ifUnmodifiedSince (Time.DateTime .GMT) where
  parse := Except.toOption ∘ Time.Format.choiceParse #[RFC822, RFC850, AscTime]
