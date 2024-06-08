namespace Http.Util

class KeyedArray.Index (k: String) (α: List String) where
  index : Fin α.length

instance : KeyedArray.Index x (x :: xs) where
  index := ⟨0, by simp⟩

instance [inst: KeyedArray.Index x xs] : KeyedArray.Index x (y :: xs) where
  index := ⟨inst.index.val + 1, by simp [List.length, Nat.succ_lt_succ]⟩

structure KeyedArray (keys: List String) (α: Type) where
  values: Array α
  proof: keys.length = values.size
  deriving Repr

def KeyedArray.push (array: KeyedArray keys α) (k: String) (v: α) : KeyedArray (keys ++ [k]) α :=
  { values := array.values.push v
  , proof := by simp [Array.size_push]; exact array.proof
  }

def KeyedArray.get (k: String) [inst: Index k α] (ka: KeyedArray α β) : β :=
  ka.values.get $ by
    rw [← ka.proof]
    exact inst.index
