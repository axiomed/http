import Lean.Data.HashMap

import Http.Data.Query

open Lean

structure Uri where
  scheme    : Option String
  authority : Option String
  path      : Option String
  query     : Option Query
  fragment  : Option String

def Uri.empty : Uri := Uri.mk none none none none none

def Uri.isEmpty (uri: Uri): Bool :=
  let isEmptyQuery := Option.getD (uri.query.map (fun q => q.isEmpty)) true
  uri.scheme.isNone && uri.authority.isNone && uri.path.isNone && isEmptyQuery && uri.fragment.isNone

instance : ToString Uri where
  toString u :=
    let scheme := Option.getD (u.scheme.map (fun s => s ++ "://")) ""
    let authority := Option.getD u.authority ""
    let path := Option.getD u.path ""
    let query := Option.getD (u.query.map toString) ""
    let fragment := Option.getD (u.scheme.map (fun s => "#" ++ s)) ""

    String.join [scheme, authority, path, query, fragment]
