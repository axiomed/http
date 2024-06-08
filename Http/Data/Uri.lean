import Lean.Data.HashMap
import Http.Data.Query

namespace Http.Data

open Lean

-- TODO: Comparisons of host names MUST be case-insensitive;
-- TODO: Comparisons of scheme names MUST be case-insensitive;
-- TODO: An empty abs_path is equivalent to an abs_path of "/".

/-! Definition of URIS using the HTTP/1.1 RFC. -/

/-- TCP number port -/
abbrev Port := UInt16

structure Uri where
  scheme    : Option String
  authority : Option String
  port      : Option Port
  path      : Option String
  query     : Option String
  fragment  : Option String
  deriving Inhabited, Repr

def Uri.empty : Uri := Uri.mk none none none none none none

def Uri.isEmpty (uri: Uri): Bool :=
  let isEmptyQuery := Option.getD (uri.query.map (fun q => q.isEmpty)) true
  uri.scheme.isNone && uri.authority.isNone && uri.path.isNone && isEmptyQuery && uri.fragment.isNone

instance : ToString Uri where
  toString u :=
    let scheme := Option.getD (u.scheme.map (fun s => s ++ "://")) ""
    let authority := Option.getD u.authority ""
    let path := Option.getD u.path ""
    let query := Option.getD (u.query.map ToString.toString) ""
    let fragment := Option.getD (u.scheme.map (fun s => "#" ++ s)) ""

    String.join [scheme, authority, path, query, fragment]
