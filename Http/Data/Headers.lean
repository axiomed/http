import Lean.Data.HashMap

namespace Http.Data

open Lean

-- | Headers are a map from header names to header values.
structure Headers where
  headers : HashMap String String

instance : Repr Headers where
  reprPrec h _ := repr h.headers.toList

instance : ToString Headers where
  toString h :=
    let headerStrings := h.headers.toList.map (fun (k, v) => k ++ ": " ++ v)
    String.intercalate "\r\n" headerStrings

def Headers.empty : Headers := { headers := HashMap.empty }

instance : Inhabited Headers where
  default := Headers.empty

def Headers.add (headers : Headers) (name : String) (value : String) : Headers :=
  { headers with headers := headers.headers.insert name value }

def Headers.with (name: String) (value: String) (headers: Headers) : Headers :=
  headers.add name value
