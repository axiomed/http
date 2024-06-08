import Lean.Data

namespace Http.Data

/-- A set of cookies -/
def Cookie.Jar := Lean.HashMap String String

namespace Cookie.Jar

/-- Creates a new empty jar of cookies -/
def empty : Jar :=
  Lean.HashMap.empty

/-- Add a new pair of key and values to the hashmap -/
def add (jar: Jar) (k v: String) : Jar :=
  Lean.HashMap.insert jar k v
