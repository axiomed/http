import Lean.Data.Parsec

namespace Http.Util.Parser
open Lean.Parsec

def tokenMap : Array Nat := #[
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

def isToken (c: Char) : Bool :=
  c.toNat < 256
  ∧ tokenMap[c.toNat.toUInt8.toNat]! = 1

def isAlpha (c : Char) : Bool :=
  (c ≥ 'a' && c ≤ 'z') || (c ≥ 'A' && c ≤ 'Z')

def isDigit (c : Char) : Bool :=
  c ≥ '0' && c ≤ '9'

def isRestrictedFirst (c : Char) : Bool :=
  isAlpha c || isDigit c

def isRestrictedChar (c : Char) : Bool :=
  isAlpha c || isDigit c || c == '!' || c == '#' || c == '$' ||
  c == '&' || c == '-' || c == '^' || c == '_' || c == '.' || c == '+'

/--
token = 1*any CHAR except CTLs or separators
separators = "(" | ")" | "<" | ">" | "@"
           | "," | ";" | ":" | "\" | <">
           | "/" | "[" | "]" | "?" | "="
           | "{" | "}" | SP | HT
-/

@[inline]
def tokenChar : Lean.Parsec Char := satisfy isToken

@[inline]
def token : Lean.Parsec String := many1Chars tokenChar

@[inline]
def restrictedNameFirst : Lean.Parsec Char :=
  satisfy isRestrictedFirst

@[inline]
def restrictedNameChar : Lean.Parsec Char :=
  satisfy isRestrictedChar

@[inline]
def restrictedName : Lean.Parsec String := do
  many1Chars restrictedNameChar

@[inline]
def sepBy1 (parser: Lean.Parsec α) (sep: Lean.Parsec β) : Lean.Parsec (Array α) :=
  (parser >>= λcookie => manyCore (sep *> parser) #[cookie])

@[inline]
def sepBy (parser: Lean.Parsec α) (sep: Lean.Parsec β) : Lean.Parsec (Array α) :=
  sepBy1 parser sep <|> pure #[]

@[inline]
def sepByComma (parser: Lean.Parsec α) : Lean.Parsec (Array α) :=
  sepBy parser (ws *> skipChar ',' <* ws)

@[inline]
def number : Lean.Parsec Nat :=
   String.toNat! <$> Lean.Parsec.many1Chars Lean.Parsec.digit

@[inline]
def float : Lean.Parsec Float := do
  let num ← number

  let dec ← optional $ do
    skipChar '.'
    let num ← Float.ofNat <$> number
    let len := Float.floor (Float.log10 (Float.abs num)) + 1
    let div := Float.pow 10 len
    return num / div

  let dec := dec.getD 0.0

  return num.toFloat + dec

@[inline]
def quotedString : Lean.Parsec String :=
  skipChar '"' *> manyChars (satisfy (· ≠ '"')) <* skipChar '"'
