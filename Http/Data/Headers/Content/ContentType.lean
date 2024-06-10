import Http.Data.Headers.Name
import Http.Classes.FromString
import Http.Data.Mime

namespace Http.Data.Headers
open Http.Classes

/-- Indicates the media type related to the content of the body of the message.

* Reference: https://httpwg.org/specs/rfc9110.html#field.content-type
-/
def ContentType := Mime

instance : Header .contentType ContentType where
  parse := Except.toOption ∘ Mime.parse
