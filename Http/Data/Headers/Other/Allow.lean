import Http.Data.Headers.Name
import Http.Data.Method

namespace Http.Data.Headers
open Http.Classes

instance : Header .allow (Array Method) where
  parse input
    := input
    |>.split (· = ',')
    |>.toArray
    |>.sequenceMap Parseable.parse
