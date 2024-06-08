import Http.Data.Headers.Name
import CaseInsensitive
import DHashMap
import Lean

namespace Http.Data
open HeaderName
open Lean

/-! Definition of a set of HTTP Headers. It is a multi map of fields -/

/-- A header value is the first value of the getType of a header name -/
abbrev HeaderValue := Sigma.fst ∘ HeaderName.getType

/-- Header values are a bunch of values that can be equal

* It MUST be possible to combine the multiple header fields into one
  "field-name: field-value" pair, without changing the semantics of the
  message, by appending each subsequent field-value to the first, each
  separated by a comma.

* The order in which header fields with the same
  field-name are received is therefore significant to the
  interpretation of the combined field value, and thus a proxy MUST NOT
  change the order of these field values when a message is forwarded. In
  this library its a ordered array in an unordered map so it will result
  in the same thing.
-/
abbrev HeaderValues := Array ∘ HeaderValue

/-- Map of case insensitive fields to multiple values -/
def Headers := Lean.DHashMap HeaderName HeaderValues
  deriving Inhabited

instance : Repr Headers where
  reprPrec h _ :=
    let headerStrings := h.toList.map fun sigma =>
      let res := sigma.snd
      let value :=
        String.intercalate ","
        $ Array.toList
        $ Array.map (λk => String.quote (sigma.fst.getType.snd.stringify k)) (by simp at res; exact res)
      s!"{String.quote (toString sigma.fst)}: {value}"
    s!"\{{String.intercalate ", " headerStrings}}"

instance : ToString Headers where
  toString h :=
    let headerStrings := h.toList.map fun sigma =>
      let res := sigma.snd
      let value :=
        String.intercalate ","
          $ Array.toList
          $ Array.map (λk => sigma.fst.getType.snd.stringify k) (by simp at res; exact res)
      s!"{toString sigma.fst}: {value}"
    String.intercalate "\r\n" headerStrings

def Headers.empty : Headers := Lean.DHashMap.empty

/-- Adds a new value to the header map -/
def Headers.add (headers: Headers) (name: HeaderName) (value: HeaderValue name) : Headers :=
  let arr := headers.findD name #[]
  let arr := arr.push value
  headers.insert name arr

/-- Adds a new value to the header map, parsing the value -/
def Headers.add? (headers: Headers) (name: HeaderName) (value: String) : Headers :=
  let arr := headers.findD name #[]
  let inst := name.getType.snd
  match inst.parse value with
  | .some res => headers.insert name (arr.push res)
  | .none => headers

def Headers.add! (headers: Headers) (name: HeaderName) (value: String) : Headers :=
  let arr := headers.findD name #[]
  let inst := name.getType.snd
  match inst.parse value with
  | .some val =>
    let arr := arr.push val
    headers.insert name arr
  | .none => headers
/-- Get the first value of a header s-/
def Headers.find? (headers: Headers) (name: HeaderName) : Option (HeaderValue name) :=
  (Array.get? · 0) =<< (DHashMap.find? headers name)

/-- Get all of the multiple values of a header -/
def Headers.findAll? (headers: Headers) (name: HeaderName) : Option (HeaderValues name) :=
  DHashMap.find? headers name
