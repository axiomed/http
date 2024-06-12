import Http.Classes.Parseable
import Http.Data.Headers.Name
import Http.Data.Uri

namespace Http.Data.Headers
open Http.Classes

instance : Header .host String where
  parse := some
