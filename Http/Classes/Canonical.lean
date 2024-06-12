import CaseInsensitive

namespace Http.Classes

inductive Encode : Type → Type where
  | text : Encode String
  | binary : Encode ByteArray

/-- The canonical representation of some data structure in the HTTP protocol as strings or binary. -/
class Canonical (encode: Encode β) (α : Type) where
  repr : α → β

namespace Canonical

instance : Canonical .text String where
  repr := id

instance : Canonical .text String.CI where
  repr x := x.value.toLower

def text [inst: Canonical .text α] : α → String := inst.repr

def binary [inst: Canonical .binary α] : α → ByteArray := inst.repr
