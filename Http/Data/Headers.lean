import Lean.Data.HashMap

namespace Http.Data
open Lean

structure Headers where
  /-- All the -/
  headers : HashMap String String

  /-- The length of the body that is being received, if it's none then its probably using
      Transfer encoding instead
  -/
  contentLength : Option Nat
  /-- The list of encodings that is going to be used to parse the body -/
  transferEncoding : Array String
  /-- If the connection should close after sending the response  -/
  close : Bool

instance : Repr Headers where
  reprPrec h _ := repr h.headers.toList

instance : ToString Headers where
  toString h :=
    let headerStrings := h.headers.toList.map (fun (k, v) => k ++ ": " ++ v)
    String.intercalate "\r\n" headerStrings

def Headers.empty : Headers :=
  { headers := HashMap.empty
  , contentLength := none
  , transferEncoding := #[]
  , close := false
  }

instance : Inhabited Headers where
  default := Headers.empty

def Headers.add (headers : Headers) (name : String) (value : String) : Headers :=
  { headers with headers := headers.headers.insert name value }

def Headers.with (name: String) (value: String) (headers: Headers) : Headers :=
  headers.add name value

def Headers.contains (name: String) (headers: Headers) : Bool :=
  headers.headers.contains name
