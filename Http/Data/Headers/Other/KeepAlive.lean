import Http.Data.Headers.Name
import Http.Data.Mime
import Http.Data.Method
import Lean.Data.Parsec
import Http.Util.Parser

namespace Http.Data.Headers
open Http.Classes Lean.Parsec Http.Util.Parser
open Lean

structure KeepAlive where
  values: Lean.HashMap String.CI String

def KeepAlive.new (timeout: Option Nat) (max: Option Nat) : KeepAlive := Id.run do
  let mut values := Lean.HashMap.empty

  if let some timeout := timeout then
    values := values.insert (String.CI.new "timeout") (toString timeout)

  if let some max := max then
    values := values.insert (String.CI.new "max") (toString max)

  return { values }

def KeepAlive.paramsParser : Lean.Parsec (String.CI × String) := do
  let name ← token
  let value ← skipChar '=' *> (quotedString <|> token)
  return (String.CI.new name, value)

def KeepAlive.parser : Lean.Parsec KeepAlive := do
  let res ← sepByComma KeepAlive.paramsParser <* eof
  let values := res.foldl (λh (k, v) => h.insert k v) HashMap.empty
  return { values }

instance : Header .keepAlive KeepAlive where
  parse := Except.toOption ∘ KeepAlive.parser.run

instance : Canonical .text KeepAlive where
  repr map :=
    let arr := map.values.toList
    let arr := arr.map (λ(k,v) => s!"{k}={v}")
    String.intercalate ", " arr
