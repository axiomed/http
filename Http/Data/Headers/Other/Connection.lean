import Http.Data.Headers.Name

namespace Http.Data.Headers
open Http.Classes

instance : Header .connection String where
  parse := some
