import Http.Data.Headers
import Http.IO.Buffer
import Http.Util

namespace Http.Protocols.Http1.Data
open Http.Util
open Http.IO

/-! HTTP [Chunk] structure that represents a single chunk of data in HTTP/1.1 -/

/-- The 'Chunk' structure represents a single chunk of data in an HTTP/1.1 chunked transfer encoding
It includes optional extensions and the actual data. -/
structure Chunk where
  extensions: Http.Data.Headers
  data: ByteArray

def Chunk.fromString (x: String) : Chunk :=
  Chunk.mk Data.Headers.empty (String.toUTF8 x)

/-- 'Trailer' is defined as a type alias for Headers, which represents the trailing headers that may
be sent after the last chunk in an HTTP/1.1 response. -/
abbrev Trailers := Http.Data.Headers

instance : ToString Chunk where
  toString s := String.fromUTF8! s.data

instance : Serialize Chunk where
  serialize chunk := do
    BufferBuilder.write (toHex chunk.data.size)
    BufferBuilder.write (toString chunk.extensions)
    BufferBuilder.write "\r\n"
    BufferBuilder.write chunk.data
    BufferBuilder.write "\r\n"
