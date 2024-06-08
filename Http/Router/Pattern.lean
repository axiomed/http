import Lean.Data.AssocList

namespace Http.Router
open Lean

inductive Pattern
  | name : String → Pattern
  | capture : String → Pattern
  | wildcard
  deriving Repr

namespace Pattern

def getCapture : Pattern → Option String
  | .capture n => some n
  | _ => none

@[simp]
def captures (pat: Array Pattern) : List (String × Nat) :=
  let caps := pat.mapIdx (·, ·) |>.filterMap (λ(idx, pat) => (·, idx.val) <$> Pattern.getCapture pat)
  caps.foldl (λl (k, v) => (k, v) :: l) []

@[simp]
def String.Iterator.takeWhile (it : String.Iterator) (f : Char → Bool) : (String.Iterator × String) :=
  let rec loop (it : String.Iterator) (acc : String) : (String.Iterator × String) :=
    if it.hasNext ∧ f it.curr ∧ it.next.remainingBytes < it.remainingBytes then
      loop (it.next) (acc.push it.curr)
    else
      (it, acc)
  termination_by it.remainingBytes
  loop it ""

@[simp]
private def parsePattern (input : String.Iterator) : Option (Pattern × String.Iterator) :=
  if input.atEnd then
    none
  else
    match input.curr with
    | ':' =>
      let rest := input.forward 1
      let (it, cap) := String.Iterator.takeWhile rest (fun c => c ≠ '/')
      some (Pattern.capture cap, it)
    | '*' =>
      some (Pattern.wildcard, input.forward 1)
    | _ =>
      let (it, nam) := String.Iterator.takeWhile input (fun c => c ≠ '/' && c ≠ ':' && c ≠ '*')
      some (Pattern.name nam, it)

@[simp]
def parse (input : String) : Option (Array Pattern) :=
  let rec parseAux (input : String.Iterator) (acc : Array Pattern) : Option (Array Pattern) := do
    if input.atEnd then
      return acc

    if let '/' := input.curr then
      let rest := input.forward 1
      let (pattern, rest) ← parsePattern rest
      if rest.remainingBytes < input.remainingBytes
        then parseAux rest (acc.push pattern)
        else some acc
    else
      some acc

  termination_by input.remainingBytes
  parseAux input.iter #[]

@[simp]
def parse! : String → Array Pattern := Option.get! ∘ parse
