import Http.Data.Uri.Query
import Http.Data.Uri.Scheme
import Http.Data.Uri.Authority

namespace Http.Data
open Lean

/-! Definition of URIS using the HTTP/1.1 RFC.

* Reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-3.2.2
-/
structure Uri where
  scheme    : Option String
  authority : Option String
  path      : Option String
  query     : Option String
  port      : Option String
  fragment  : Option String
  deriving BEq, Repr, Inhabited

instance : ToString Uri where
  toString u :=
    let scheme := Option.getD (u.scheme.map (fun s => toString s ++ "://")) ""
    let authority := Option.getD (u.authority.map toString) ""
    let path := Option.getD u.path ""
    let query := Option.getD (u.query.map toString) ""
    let fragment := Option.getD (u.scheme.map (fun s => "#" ++ toString s)) ""
    String.join [scheme, authority, path, query, fragment]

namespace Uri

def empty : Uri := Uri.mk none none none none none none
