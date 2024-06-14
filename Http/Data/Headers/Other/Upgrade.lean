import Http.Data.Headers.Name

namespace Http.Data.Headers
open Http.Classes

inductive UpgradeHeader.Standard where
  | websocket
  | h2
  deriving Inhabited, BEq, Repr, Hashable

inductive UpgradeHeader where
  | standard (val: UpgradeHeader.Standard)
  | custom (value: String)
  deriving Inhabited, BEq, Repr, Hashable

private def upgradeStandard : Lean.Data.Trie UpgradeHeader.Standard :=
  Lean.Data.Trie.empty
  |>.insert "websocket" .websocket
  |>.insert "h2" .h2

instance standard.Parseable : Parseable UpgradeHeader.Standard where
  parse name := upgradeStandard.find? name.toLower

instance standard.Canonical : Canonical .text UpgradeHeader.Standard where
  repr
    | .websocket => "websocket"
    | .h2 => "h2"

instance : Parseable UpgradeHeader where
  parse name := upgradeStandard.find? name
    |>.map (.standard)
    |>.orElse (λ_ => some (UpgradeHeader.custom name))

instance : Canonical .text UpgradeHeader where
  repr
    | .standard s => Canonical.text s
    | .custom v   => v

instance : Header .upgrade UpgradeHeader where
  parse := Parseable.parse
