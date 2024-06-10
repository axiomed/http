import Http.Classes.Standard
import Http.Data.Mime.Basic
import Lean.Data.AssocList
import Lean.Data.Parsec
import Http.Util.Parser

namespace Http.Data.Mime
open Http.Util.Parser
open Http.Classes
open Lean.Parsec
open Lean

private def typeName : Lean.Parsec String :=
  restrictedName

private def subtypeName : Lean.Parsec String :=
  restrictedName

private def param : Lean.Parsec (String.CI × String) := do
  skipChar ';'
  ws
  let name ← token
  skipChar '='
  let value ← token
  pure (String.CI.new name, value)

private def parseParams : Lean.Parsec (AssocList String.CI String) := do
  let params ← many param
  return params.foldl (λx (k, v) => x.insert k v) Inhabited.default

private def mimeType : Lean.Parsec Mime := do
  let type ← typeName
  skipChar '/'
  let subType ← subtypeName
  let params ← parseParams
  pure { type := Standard.parse type, subType := String.CI.new subType, params }

private def any (parser: Lean.Parsec α) : Lean.Parsec (Any α) :=
  Lean.Parsec.orElse (skipChar '*' *> pure Any.any)
                     (λ_ => parser <&> Any.specific)

private def mimeRange : Lean.Parsec MediaRange := do
  let type ← any $ Standard.parse <$> typeName
  skipChar '/'
  let subType ← any $ String.CI.new <$> subtypeName
  let params ← parseParams
  pure { type, subType, params }


/-! Parses a Media type from a string according to RFC 2045 Section 5.1.

* Reference: https://datatracker.ietf.org/doc/html/rfc2045#section-5.1
-/
def parse (s: String) : Except String Mime :=
  mimeType.run s

def parseRange (s: String) : Except String MediaRange :=
  mimeRange.run s
