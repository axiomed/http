import Lake
open Lake DSL
open Lean

-- Extra definitions

def mkArrayLit (lvl : Level) (type : Expr) (l : List Expr) : Expr :=
  let empty := Expr.app (Expr.const ``Array.empty [lvl]) type
  let push r h := mkAppN (Expr.const ``Array.push [lvl]) #[type, r, h]
  l.foldl push empty

def elabRunPkgConfig (stx : Syntax) (args : Array String) : Elab.TermElabM Expr := do
  Lean.withRef stx do
    match ← (IO.Process.output { cmd := "pkg-config", args }).toBaseIO with
    | .ok out =>
      if out.exitCode != 0 then
        throwErrorAt stx "pkg-config failed: {out.exitCode}"
      let libParts := out.stdout.splitOn
      let stringType := Expr.const ``String []
      libParts
          |>.map (mkStrLit ·.trimRight)
          |> mkArrayLit .zero stringType
          |> pure
    | .error _ =>
        throwErrorAt stx "Could not run pkg-config"

syntax:lead (name := libuvLibsElab) "libuvLibs" : term

@[term_elab libuvLibsElab]
def elabLibUVLibs : Lean.Elab.Term.TermElab := fun stx _expectedType? =>
  elabRunPkgConfig stx #["--libs", "libuv"]

package «example» where
  -- add package configuration options here

lean_lib «Example» where
  -- add library configuration options here

@[default_target]
lean_exe exampl where
  root := `Main
  moreLinkArgs := libuvLibs

require Http from ".."
