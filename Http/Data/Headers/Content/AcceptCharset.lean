import Http.Classes.FromString
import Http.Data.Headers.Name
import Http.Data.Mime
import Http.Util.Parser

namespace Http.Data.Headers
open Http.Classes Http.Util.Parser
open Lean.Parsec

inductive CharSet where
  | custom (val: String)
  | any
  deriving Repr

/-- Charset accepted by the agent.

* Reference: https://httpwg.org/specs/rfc9110.html#field.accept-charset
-/
structure AcceptCharSet where
  name: CharSet
  weight: Option Float
  deriving Repr

def AcceptCharSet.parser : Lean.Parsec AcceptCharSet := do
  let name ← (token <&> .custom)
          <|> (skipString "*" *> pure .any)

  let weight ← optional (skipChar ';' *> ws *> skipString "q=" *> float)

  return { name, weight }

def AcceptCharSet.parse : String → Option AcceptCharSet :=
  Except.toOption ∘ AcceptCharSet.parser.run

instance : Header .acceptCharset AcceptCharSet where
  parse := AcceptCharSet.parse
