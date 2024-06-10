import Http.Data.Headers.Name

namespace Http.Data.Headers

instance : Header .contentLocation String where
  parse := some
