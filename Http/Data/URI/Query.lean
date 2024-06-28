import Http.Classes
import Lean.Data.Parsec
import Http.Util.Parser

namespace Http.Data.Uri
open Http.Util.Parser
open Http.Classes
open Lean.Parsec

/-! List of query parameters in a URL -/

structure Query where
  pairs : Array (String × Option String)
  deriving BEq, Repr, Inhabited

def Query.empty : Query :=
  Query.mk Array.empty

def Query.isEmpty (query : Query) : Bool :=
  query == Query.empty

instance : Canonical .text Query where
  repr q :=
    let flat : (String × Option String) → String
      | (k, some v) => k ++ "=" ++ v
      | (k, none) => k

    let pairs := q.pairs.map flat
    String.join (pairs.toList.intersperse "&")

def Query.parsePair : Lean.Parsec (String × Option String) := do
  let key ← manyChars (satisfy fun c => c != '=' && c != '&')
  _ ← optional (pchar '=')
  let value ← optional (manyChars (satisfy fun c => c != '&'))
  return (key, value)

def parseQuery : Lean.Parsec Query := do
  let pairs ← sepBy Query.parsePair (pchar '&')
  return { pairs }

def Query.fromString (s : String) : Option Query :=
  match parseQuery.run s with
  | .ok res  => some res
  | .error _ => none
