import Http.Classes.Standard
import Lean.Data.Trie
import Http.Data.Headers
import Http.IO.Buffer
import Http.Util.Format

namespace Http.Protocols.Http1.Data
open Http.Util.Format
open Http.Classes
open Http.Data
open Http.IO

/-! HTTP [Chunk] structure that represents a single chunk of data in HTTP/1.1 -/

/-- The 'Chunk' structure represents a single chunk of data in an HTTP/1.1 chunked transfer encoding
It includes optional extensions and the actual data. -/
structure Chunk where
  extensions: Headers
  data: ByteArray

instance : Serialize Chunk where
  serialize chunk := do
    BufferBuilder.write (toHex chunk.data.size)
    BufferBuilder.write (Canonical.text chunk.extensions)
    BufferBuilder.write "\r\n"
    BufferBuilder.write chunk.data
    BufferBuilder.write "\r\n"

/-- Transforms a UTF8 encoded string to a ByteArray and inserts into a Chunk-/
def Chunk.fromString (x: String) : Chunk :=
  Chunk.mk Data.Headers.empty (String.toUTF8 x)

/-- Generates a chunk with the bytearray empty so it signals the end block of a chunked request -/
def Chunk.zeroed : Chunk :=
  Chunk.mk Inhabited.default ByteArray.empty

abbrev TrailersImp := Headers

/-- These are headers that -/
def Trailers.badHeadersTrie : Lean.Data.Trie Unit :=
  Lean.Data.Trie.empty
  |>.insert "authorization" ()
  |>.insert "cache-control" ()
  |>.insert "connection" ()
  |>.insert "content-encoding" ()
  |>.insert "content-length" ()
  |>.insert "content-range" ()
  |>.insert "content-type" ()
  |>.insert "expect" ()
  |>.insert "host" ()
  |>.insert "keep-alive" ()
  |>.insert "max-forwards" ()
  |>.insert "pragma" ()
  |>.insert "proxy-authenticate" ()
  |>.insert "proxy-authorization" ()
  |>.insert "proxy-connection" ()
  |>.insert "range" ()
  |>.insert "realm" ()
  |>.insert "te" ()
  |>.insert "trailer" ()
  |>.insert "transfer-encoding" ()
  |>.insert "www-authenticate" ()

def TrailersImp.empty : TrailersImp := Headers.empty

/-- Check if the Trailer name is valid -/
def TrailersImp.valid (name: String.CI) : Bool :=
  ¬ name.value.startsWith "If-"
  ∧ Option.isNone (Trailers.badHeadersTrie.find? name.value)

def TrailersImp.add (trailers: TrailersImp) (name: String) (value: String) : TrailersImp :=
  let ci := String.CI.new name
  if TrailersImp.valid ci
    then Headers.addRaw trailers (Standard.parse name) value
    else trailers

inductive TrailersImp.WellFormed : TrailersImp → Prop where
  | mkWff  :                         WellFormed .empty
  | addWff : ∀ m a b, WellFormed m → WellFormed (.add m a b)

/-- 'Trailer' is defined as a type alias for Headers, which represents the trailing headers that may
be sent after the last chunk in an HTTP/1.1 response. -/
def Trailers := Subtype TrailersImp.WellFormed

/-- Creates a empty Trailers headers -/
def Trailers.empty : Trailers :=
  ⟨TrailersImp.empty, .mkWff⟩

/-- Adds a value in the trailers -/
def Trailers.add (trailers: Trailers) (k v: String) : Trailers :=
  ⟨trailers.val.add k v, .addWff trailers.val k v trailers.property⟩

instance : Inhabited Trailers where
  default := Trailers.empty
