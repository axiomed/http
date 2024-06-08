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

require CaseInsensitive from git "https://github.com/axiomed/CaseInsensitive.lean.git"
require DHashMap from git "https://github.com/axiomed/DHashMap.lean.git"
require alloy from git "https://github.com/tydeu/lean4-alloy.git"
require LibUV from git "https://github.com/algebraic-sofia/lean-libuv.git" @ "socket-fix"
require Parse from git "https://github.com/axiomed/Parse.lean.git"
require Time from git "https://github.com/axiomed/Time.lean.git"

meta if get_config? env = some "dev" then
  require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
