import Lean.Elab.Deriving.Basic
import Lean.Elab.Deriving.Util

namespace Http.Util.Elab
open Lean.Elab Command Term Lean Parser Command Std
open Lean.Parser.Term
open Lean.Meta

def mkStrLit (str: String) : Lean.TSyntax `str :=
  Lean.TSyntax.mk $ Lean.Syntax.mkStrLit str

def newIdent : String → Ident := Lean.mkIdent ∘ Name.mkStr1
