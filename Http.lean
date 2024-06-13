import Http.Data
import Http.IO.Connection
import Http.IO.Server

namespace Http

export Http.IO (Connection)
export Http.IO.Server (server)
export Http.Protocols.Http1.Data (Chunk Trailers Chunk.fromString)

instance : Coe String String.CI where
  coe := String.CI.new
