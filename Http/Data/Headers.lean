import Lean.Data.HashMap

namespace Http.Data
open Lean

/-! Definition of a set of HTTP Headers. It is a multi map of fields -/

/-- Map of case insensitive fields to multiple values -/
structure Headers where
  headers : HashMap String (Array String)
  deriving Inhabited

instance : ToString Headers where
  toString h :=
    let headerStrings := h.headers.toList.map (fun (k, v) => k ++ ": " ++ v.get! 0)
    String.intercalate "\r\n" headerStrings

/-- Creates a new empty set of HTTP headers -/
def Headers.empty : Headers :=
  { headers := Inhabited.default }

/-- Adds a new value to the header map -/
def Headers.add (headers: Headers) (name: String) (value: String) : Headers :=
  let arr := headers.headers.findD name.toLower #[]
  let arr := arr.push value
  { headers := headers.headers.insert name arr}

/-- Get the first value of a header s-/
def Headers.find? (headers: Headers) (name: String) : Option String :=
  (Array.get! · 0) <$> (headers.headers.find? name.toLower)

/-- Get all of the multiple values of a header -/
def Headers.findAll? (headers: Headers) (name: String) : Option (Array String) :=
  headers.headers.find? name.toLower
