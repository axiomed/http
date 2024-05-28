import Lake
open Lake DSL

package Http where

module_data alloy.c.o.export : BuildJob FilePath
module_data alloy.c.o.noexport : BuildJob FilePath

open Lean
lean_lib Http where
  precompileModules := true
  nativeFacets := fun shouldExport =>
    if shouldExport then
      #[Module.oExportFacet, `alloy.c.o.export]
    else
      #[Module.oNoExportFacet, `alloy.c.o.noexport]

require alloy from git "https://github.com/tydeu/lean4-alloy.git"
require LibUV from git "https://github.com/algebraic-sofia/lean-libuv.git" @ "socket-fix"
require Parse from "../lean-parse"
