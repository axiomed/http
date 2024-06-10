import Http.Data.Headers.Name

namespace Http.Data.Headers

instance : Header .ifMatch String where
  parse := some
