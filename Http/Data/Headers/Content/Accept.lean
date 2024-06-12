import Http.Classes.Parseable
import Http.Data.Headers.Name
import Http.Data.Mime

namespace Http.Data.Headers
open Http.Classes

instance : Header .accept MediaRange where
  parse := Except.toOption ∘ Mime.parseRange
