import Http.Classes.Parseable
import Lean.Data.Trie

namespace Http.Classes

/-- Standard type class is useful for things that have a custom and a standard string representation
and can be parsed from string to one of them.
-/
class Standard (α : Type) (β: outParam Type) where
  custom : String → α
  standard : β → α

def Standard.parse [str: Parseable β] [inst: Standard α β] (input: String) : α :=
  match str.parse input with
  | some res => inst.standard res
  | none => inst.custom input
