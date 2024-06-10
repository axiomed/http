import Http.Data.Mime

namespace Http.Data

/-- The body of a request or a response. It contains the mime type of the output so, if the data is
from a data type that needs to be chunked, it will be chunked and the length that can be useful to
the Content-Length header. -/
structure Body (stream: Type) where
  mime: Mime
  length: Option UInt64
  stream: stream
