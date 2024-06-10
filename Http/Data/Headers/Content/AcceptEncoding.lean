import Http.Classes.FromString
import Http.Data.Headers.Name
import Http.Data.Mime
import Http.Util.Parser

namespace Http.Data.Headers
open Http.Classes Http.Util.Parser
open Lean.Parsec

inductive Coding where
  | custom (val: String)
  | identity
  | any
  deriving Repr

/-- indicate an encoding transformation that has been or can be applied to a representation.

* Reference: https://httpwg.org/specs/rfc9110.html#field.accept-encoding
-/
structure ContentCoding where
  name: Coding
  weight: Option Float
  deriving Repr

def ContentCoding.parser : Lean.Parsec ContentCoding := do
  let name ← (skipString "identity" *> pure .identity)
          <|> (token <&> .custom)
          <|> (skipString "*" *> pure .any)

  let weight ← optional (skipChar ';' *> ws *> skipString "q=" *> float)

  return { name, weight }

def ContentCoding.parse : String → Option ContentCoding :=
  Except.toOption ∘ ContentCoding.parser.run

instance : Header .acceptEncoding ContentCoding where
  parse := ContentCoding.parse
