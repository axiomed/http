import Soda.Grape.Text

open Lean

-- | A version of the HTTP protocol.
structure Version where
  major : Nat
  minor : Nat
  deriving Repr, BEq

-- | https://datatracker.ietf.org/doc/html/rfc1945
def Version.v10 := Version.mk 1 0

-- | https://datatracker.ietf.org/doc/html/rfc2616
def Version.v11 := Version.mk 1 1

def Version.v20 := Version.mk 2 0

def Version.parser : Grape.Grape Version := do
  let major ← Grape.Text.number
  let _ ← Grape.chr '.'
  let minor ← Grape.Text.number
  Grape.pure { major, minor }

instance : ToString Version where
  toString v :=
    let tail := if v == Version.v20 then "2" else toString v.major ++ "." ++ toString v.minor
    "HTTP/" ++ tail

