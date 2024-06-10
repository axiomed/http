import Http.Data.Headers.Auth.Authorization
import Http.Data.Headers.Name
import Http.Classes.FromString
import Http.Util.Parser
import Lean.Data.Parsec

namespace Http.Data.Headers
open Lean.Parsec
open Http.Util.Parser
open Http.Classes

/-- Challanges to gain access to a resource

* Reference: https://datatracker.ietf.org/doc/html/rfc7235#section-4.1
-/
structure Challenge where
  scheme : AuthorizationScheme
  params : Array (String × String)
  deriving Repr

instance : Canonical Challenge where
  repr challenge :=
    let paramsStr := challenge.params.map (fun (k, v) => s!"{k}={v}")
    let paramsStr := String.intercalate ", " paramsStr.toList
    if paramsStr.isEmpty then
      Canonical.repr challenge.scheme
    else
      s!"{Canonical.repr challenge.scheme} {paramsStr}"

def Challenge.parser : Lean.Parsec Challenge := do
  let scheme ← AuthorizationScheme.parser
  let params ← sepBy (do
    ws
    let name ← token
    skipChar '='
    let value ← quotedString <|> token
    pure (name, value))
    (skipChar ',')
  return { scheme, params }

def Challenge.parse : String → Except String Challenge :=
  (Challenge.parser <* eof).run

instance : Header .wwwAuthenticate Challenge where
  parse := Except.toOption ∘ Challenge.parse
