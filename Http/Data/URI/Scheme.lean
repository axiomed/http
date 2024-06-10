import CaseInsensitive
import Http.Classes.Canonical

namespace Http.Data.Uri
open Http.Classes

/-- The standard schemes described in the HTTP core semantics. -/
inductive Scheme.Standard
  | http
  | https
  deriving BEq, Repr, Inhabited

instance : ToString Scheme.Standard where
  toString
    | .http => "http"
    | .https => "https"

/-- The scheme refers to a specification for the rest of the URI. It assing meaning for part of it.
* Reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-3.1
-/
inductive Scheme
  | standard (value: Scheme.Standard)
  | custom (s: String.CI)
  deriving BEq, Repr, Inhabited

def Scheme.defaultPort : Scheme → Option Nat
  | .standard .http => some 80
  | .standard .https => some 443
  | .custom _ => none

instance : ToString Scheme where
  toString
    | .standard std => toString std
    | .custom s => toString s

instance : Canonical Scheme where
  repr := toString
