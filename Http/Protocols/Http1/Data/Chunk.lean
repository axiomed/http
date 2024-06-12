import Http.Classes.Standard
import Lean.Data.Trie
import Http.Data.Headers
import Http.Util.Format

namespace Http.Protocols.Http1.Data
open Http.Util.Format
open Http.Classes
open Http.Data

/-! HTTP [Chunk] structure that represents a single chunk of data in HTTP/1.1 -/

/-- The 'Chunk' structure represents a single chunk of data in an HTTP/1.1 chunked transfer encoding
It includes optional extensions and the actual data. -/
structure Chunk where
  extensions: Headers
  data: ByteArray

instance : Canonical .binary Chunk where
  repr chunk :=
    let str := (toHex chunk.data.size).toUTF8
    let str := str.append (Canonical.text chunk.extensions).toUTF8
    let str := str.append "\r\n".toUTF8
    let str := str.append chunk.data
    let str := str.append "\r\n".toUTF8
    str

/-- Transforms a UTF8 encoded string to a ByteArray and inserts into a Chunk-/
def Chunk.fromString (x: String) : Chunk :=
  Chunk.mk Data.Headers.empty (String.toUTF8 x)

/-- Generates a chunk with the bytearray empty so it signals the end block of a chunked request -/
def Chunk.zeroed : Chunk :=
  Chunk.mk Inhabited.default ByteArray.empty
