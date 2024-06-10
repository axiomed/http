import Http.Data.Headers.Name
import Http.Data.Method

namespace Http.Data.Headers
open Http.Classes

instance : Header .allow Method where
  parse := Method.fromString
