namespace Http.Util.Format

def toHexChar (n : Nat) : Char :=
  if n < 10
    then Char.ofNat (n + 48)
    else Char.ofNat (n + 87)

def toHex (n : Nat) : String := Id.run do
  let mut value := n
  let mut result := #[]
  if value == 0 then return "0"

  while value > 0 do
    let digit := value % 16
    result := result.push $ toHexChar digit
    value := value / 16

  return result.reverse.foldl String.push ""

def isHexDigit (c : Char) : Bool :=
  ('0' ≤ c ∧ c ≤ '9') ∨
  ('A' ≤ c ∧ c ≤ 'F') ∨
  ('a' ≤ c ∧ c ≤ 'f')

def fromHexDigit! (c : Char) : Nat :=
  if '0' ≤ c ∧ c ≤ '9' then (c.toNat - '0'.toNat)
  else if 'A' ≤ c ∧ c ≤ 'F' then (c.toNat - 'A'.toNat + 10)
  else if 'a' ≤ c ∧ c ≤ 'f' then (c.toNat - 'a'.toNat + 10)
  else panic "out of bounds"

def fromHexDigit? (c : Char) : Option Nat :=
  if '0' ≤ c ∧ c ≤ '9' then some (c.toNat - '0'.toNat)
  else if 'A' ≤ c ∧ c ≤ 'F' then some (c.toNat - 'A'.toNat + 10)
  else if 'a' ≤ c ∧ c ≤ 'f' then some (c.toNat - 'a'.toNat + 10)
  else none

def fromHexNum (s: Substring) : UInt8 := Id.run do
  let mut result := 0

  for char in s do
    result := result * 16 + (fromHexDigit! char).toUInt8

  return result

def Char.fromHex : Substring → Char := Char.ofNat ∘ UInt8.toNat ∘ fromHexNum
