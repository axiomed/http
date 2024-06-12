import Http.Classes.Parseable
import Http.Data.Headers.Name
import Http.Data.Mime
import Http.Util.Parser

namespace Http.Data.Headers
open Http.Classes Http.Util.Parser
open Lean.Parsec

/-- Languages accepted by the agent.

* Reference: https://httpwg.org/specs/rfc9110.html#field.accept-language
-/
structure LanguageRange where
  name: String
  weight: Option Float
  deriving Repr

def LanguageRange.parser : Lean.Parsec LanguageRange := do
  let name ← token
  let weight ← optional (skipChar ';' *> ws *> skipString "q=" *> float)
  return { name, weight }

def LanguageRange.parse : String → Option LanguageRange :=
  Except.toOption ∘ LanguageRange.parser.run

instance : Header .acceptLanguage LanguageRange where
  parse := LanguageRange.parse
