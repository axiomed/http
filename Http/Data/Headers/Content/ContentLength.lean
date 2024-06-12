import Http.Classes.Parseable
import Http.Data.Headers.Name

namespace Http.Data.Headers
open Http.Classes

/-- When a message dont have Transfer-Encoding a content-length can be provided providing the decimal
number of octects that a message body will have.

* Reference: https://httpwg.org/specs/rfc9112.html#body.content-length
-/
def ContentLength := Nat

instance : Header .contentLength ContentLength where
  parse := some ∘ String.toNat!
