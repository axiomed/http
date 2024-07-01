import CaseInsensitive
import Http.Classes

namespace Http.Data.URI
open Http.Classes

/-- The standard schemes described in the HTTP core semantics. -/
inductive Scheme.Standard
  | http
  | https
  | ws
  | wss
  deriving BEq, Repr, Inhabited

instance : Canonical .text Scheme.Standard where
  repr
    | .http => "http"
    | .https => "https"
    | .ws => "ws"
    | .wss => "wss"

/-- The scheme refers to a specification for the rest of the URI. It assing meaning for part of it.
* Reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-3.1
-/
inductive Scheme
  | standard (value: Scheme.Standard)
  | custom (s: String.CI)
  deriving BEq, Repr, Inhabited

instance : Canonical .text Scheme where
  repr
    | .standard std => Canonical.text std
    | .custom s => Canonical.text s

instance : Parseable Scheme where
  parse
    | "http" => some (.standard .http)
    | "https" => some (.standard .https)
    | text => some (.custom (String.CI.new text))

def Scheme.defaultPort : Scheme → Option Nat
  | .standard .http => some 80
  | .standard .ws => some 80
  | .standard .https => some 443
  | .standard .wss => some 443
  | .custom _ => none
