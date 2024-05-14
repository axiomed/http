namespace Http.Data

open Lean

structure Version where
  major : Nat
  minor : Nat
  deriving Repr, BEq, Inhabited

def Version.v10 := Version.mk 1 0
def Version.v11 := Version.mk 1 1
def Version.v20 := Version.mk 2 0

instance : ToString Version where
  toString v :=
    let tail := if v == Version.v20 then "2" else toString v.major ++ "." ++ toString v.minor
    "HTTP/" ++ tail
