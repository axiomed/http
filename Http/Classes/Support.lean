import Lean.Data.Trie

namespace Http.Classes

/-- Type class to check if something is supported by the list xs.-/
class Supports (x : α) (xs: List α) where
  proof : x ∈ xs

instance : Supports x (x :: xs) where
  proof := List.Mem.head xs

instance [s: Supports x xs] : Supports x (y :: xs) where
  proof := List.Mem.tail y s.proof
