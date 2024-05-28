import Http.Data.Headers

namespace Http.Data

structure Chunk where
  extensions: Headers
  data: String

def Trailer := Headers
