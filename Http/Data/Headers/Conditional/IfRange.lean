import Http.Data.Headers.Name
import Http.Util.Date
import Time

namespace Http.Data.Headers
open Http.Util.Date

instance : Header .ifRange String where
  parse := some
