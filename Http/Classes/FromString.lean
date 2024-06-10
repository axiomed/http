import Lean.Elab.Deriving.Basic
import Lean.Elab.Deriving.Util
import Lean.Data.Trie
import Http.Util.Elab

namespace Http.Classes
open Lean.Elab Command Term Lean Parser Command Std
open Lean.Parser.Term
open Http.Util.Elab
open Lean.Meta

class FromString (α: Type) where
  trie : Lean.Data.Trie α

def FromString.fromString [inst: FromString α] : String → Option α :=
  inst.trie.find?

def mkInsert (f: Lean.TSyntax `term) (name: String) : CommandElabM (Lean.TSyntax `term) :=
  `(($f |>.insert $(mkStrLit name) .$(newIdent name)))

def mkFromString (declName : Name) : CommandElabM Bool := do
  if !isStructure (← getEnv) declName then
    let indVal ← getConstInfoInduct declName
    let names := indVal.ctors.map (λx => x.getString!)
    let empty ← `(Lean.Data.Trie.empty)
    let res ← names.foldlM (λx n => mkInsert x n) empty
    elabCommand =<< `(
      instance : FromString $(mkIdent declName) where
        trie := $res
    )
    pure true
  else
    pure false

def mkFromStringInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  declNames.foldlM (fun b n => andM (pure b) (mkFromString n)) true

initialize registerDerivingHandler ``FromString mkFromStringInstanceHandler
