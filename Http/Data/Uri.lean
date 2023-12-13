import Lean.Data.HashMap

open Lean

structure Uri where
  scheme : Option String
  authority : Option String
  path : Option String
  query : HashMap String String
  fragment : Option String

def Uri.empty : Uri := Uri.mk none none none HashMap.empty none

instance : ToString Uri where
  toString u :=
    let scheme := Option.getD (u.scheme.map (fun s => s ++ "://")) ""
    let authority := Option.getD u.authority ""
    let path := Option.getD u.path ""
    let query := ""   -- TODO
    let fragment := Option.getD (u.scheme.map (fun s => "#" ++ s)) ""

    String.join [scheme, authority, path, query, fragment]
