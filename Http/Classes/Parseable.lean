import Lean.Elab.Deriving.Basic
import Lean.Elab.Deriving.Util
import Lean.Data.Trie
import Http.Util.Elab

namespace Http.Classes
open Lean.Elab Command Term Lean Parser Command Std
open Lean.Parser.Term
open Http.Util.Elab
open Lean.Meta

/-- Type class to convert a string to a data constructor -/
class Parseable (α: Type) where
  parse : String → Option α
