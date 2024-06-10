namespace Http.Classes

/-- The canonical representation of some data structure in the HTTP protocol. -/
class Canonical (α : Type) where
  repr : α → String
