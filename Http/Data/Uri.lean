import Http.Data.URI.Authority
import Http.Data.URI.Scheme
import Http.Data.URI.Query
import Http.Util.Format
import Http.Util
import Http.Classes

namespace Http.Data
open Http.Util.Format
open Http.Classes
open Lean

/-! Definition of URIS using the HTTP/1.1 RFC.

* Reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-3.2.2
-/
structure URI where
  authority : URI.Authority := {}
  path      : Option String := none
  query     : Option String := none
  fragment  : Option String := none
  deriving BEq, Repr, Inhabited

def URI.encode (input: String) : String :=
    input.toUTF8.foldl (λs c => s ++ encodeChar c) ""
  where
    encodeChar char :=
      if (char ≥ 97 ∧ char ≤ 122) ∨ (char ≥ 65 ∧ char ≤ 90) ∨ (char ≥ 48 ∧ char ≤ 57)
        then (Char.ofNat (UInt8.toNat char)).toString
        else s!"%{(toHex char.toNat).toUpper}"

def URI.componentDecode (input: String) : Option String := Id.run do
  let mut result := ByteArray.empty
  let mut acc := 0
  let mut start := none
  let mut endIdx := 0

  for char in input.toSubstring do
    if let some startIdx := start then
      if startIdx + 3 = endIdx then
        start := none
        result := result.push acc
        acc := 0
      else
        if let some hex := fromHexDigit? char then
          acc := acc * 16 + hex.toUInt8
        else
          return none

    if start.isNone then
      if char = '%' then
        start := some endIdx
      else
        result := result.append (String.toUTF8 char.toString)

    endIdx := endIdx + 1

  if let some startIdx := start then
    if startIdx + 3 = endIdx then
      start := none
      result := result.push acc
      acc := 0
    else
      return none

  return String.fromAscii result

instance : Canonical .text URI where
  repr u :=
    let authority := Canonical.text u.authority
    let path := Option.getD u.path ""
    let query := Option.getD u.query ""
    let fragment := Option.getD (u.fragment.map (fun s => "#" ++ s)) ""
    let notEncoded := String.join [authority, path, query, fragment]
    notEncoded

namespace URI

def empty : URI := URI.mk Inhabited.default none none none
