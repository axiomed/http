import Http.Data.Headers.Name

namespace Http.Data.Headers

instance : Header .contentEncoding String where
  parse := some
