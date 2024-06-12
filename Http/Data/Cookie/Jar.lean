import Http.Data.Cookie.Basic
import Lean.Data.HashMap

namespace Http.Data.Cookie
open Lean

/-- A Jar.Entry contains a cookie, the time it was created, the time it was last accessed, and a
sequence number to track the order of addition. -/
structure Jar.Entry where
  cookie: Cookie
  created: Time.DateTime .GMT
  lastAccess: Time.DateTime .GMT
  sequence: Nat
  deriving Repr

/-- A Jar is a collection of Cookie with the created and last access attached to them. -/
structure Jar where
  data: HashMap String Jar.Entry
  sequence: Nat

namespace Jar

/-- Initializes an empty Jar with a sequence number starting at 0. -/
def empty : Jar :=
  { data := HashMap.empty, sequence := 0 }

/-- Adds a cookie to the Jar. If a cookie with the same name already exists, it updates the existing
cookie and refreshes the last access time. The sequence number is incremented for each addition. -/
def add (jar : Jar) (cookie : Cookie) (now : Time.DateTime .GMT) : Jar :=
  let entry : Jar.Entry :=
    { cookie := cookie,
      created := now,
      lastAccess := now,
      sequence := jar.sequence }
  { jar with
    data := jar.data.insert cookie.name entry,
    sequence := jar.sequence + 1 }

/-- Retrieves a cookie from the Jar by its name. If found, it also updates the last access time. -/
def get (jar : Jar) (name : String) (now : Time.DateTime .GMT) : Option (Cookie × Jar) :=
  match jar.data.find? name with
  | some entry =>
      let updatedEntry := { entry with lastAccess := now }
      let updatedJar := { jar with data := jar.data.insert name updatedEntry }
      some (entry.cookie, updatedJar)
  | none => none

/-- Removes a cookie from the Jar by its name. -/
def remove (jar : Jar) (name : String) : Jar :=
  { jar with data := jar.data.erase name }
