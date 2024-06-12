import Http.Data.Cookie
import Http.Data.Headers.Name
import Http.Classes.Parseable
import Http.Util.Date

namespace Http.Data.Headers
open Http.Util.Date
open Http.Classes

instance : Header .setCookie Cookie where
  parse := Except.toOption ∘ Cookie.parseSet
