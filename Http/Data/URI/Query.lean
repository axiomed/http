namespace Http.Data.Uri

/-! List of query parameters in a URL -/

structure Query where
  pairs : Array (String × Option String)
  deriving BEq, Repr, Inhabited

def Query.empty : Query :=
  Query.mk Array.empty

def Query.isEmpty (query : Query) : Bool :=
  query == Query.empty

instance : ToString Query where
  toString q :=
    let flat : (String × Option String) → String
      | (k, some v) => k ++ "=" ++ v
      | (k, none) => k

    let pairs := q.pairs.map flat
    String.join (pairs.toList.intersperse "&")
