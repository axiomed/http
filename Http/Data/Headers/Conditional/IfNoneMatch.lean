import Http.Data.Headers.Name

namespace Http.Data.Headers

instance : Header .ifNoneMatch String where
  parse := some
