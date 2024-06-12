import Http.Classes.Standard
import Lean.Data.Trie
import Http.Data.Headers
import Http.Util.Format

namespace Http.Protocols.Http1.Data
open Http.Util.Format
open Http.Classes
open Http.Data

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
