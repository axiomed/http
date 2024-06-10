import Http.Data.Headers.Name
import Http.Data.Mime

namespace Http.Data.Headers
open Http.Classes

def Age := Nat

instance : Header .age Age where
  parse := String.toNat?
