# Http.lean

A WIP implementation of HTTP protocol in [Lean 4](https://github.com/leanprover/lean4).

## The Goal

The goal of this project is to provide Lean community an HTTP Client and Server implementations as well
as working and sensible set of HTTP primitives.

The broader goal of [Axiomed](https://github.com/axiomed) initiative is to build an ecosystem of tools in Lean 4 that
software engineers need in their everyday tasks, such as HTTP libs, async frameworks, DB connectors, parsers etc.

If you think Lean 4 can shine not only as a theorem prover, but as a general purpose language - you're welcome to 
join the organization!

## Usage

Add Http to your `lakefile.lean`:

```lean
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

@[default_target]
lean_exe «change-the-name-of-your-application-here-it-needs-the-more-link-args» where
  root := `Main
  moreLinkArgs := libuvLibs

require Http from git "https://github.com/axiomed/Http.lean.git"
```

Example:

```lean
import Http

open Http.Data
open Http

def onConn (conn: Connection) : IO Unit := do
  -- Using defined headers with the formats that are assigned to them.
  conn.withHeaderStd .date (← Time.DateTime.now .GMT)
  conn.withHeaderStd .contentType (Mime.mk (.standard .text) "plain" (.empty |>.insert "charset" "utf-8"))

  -- Using a raw header that can be invalid for the client.
  conn.withHeader "transfer-encoding" "chunked"

  -- Send the headers so it can determine the content-type and stuff
  conn.sendHeaders

  -- Write strings to the response
  conn.write "some message"
  conn.write " here!"

  -- Ends the request.
  conn.end

def main : IO Unit :=
  Http.IO.Server.server
    (host := "127.0.0.1")
    (port := 8081)
    (onEnd := λconn _ => onConn conn)
```

## Acknowledgements

This library is based on work @algebraic-sofia has done for her
