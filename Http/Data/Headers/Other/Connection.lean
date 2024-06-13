import Http.Data.Headers.Name

namespace Http.Data.Headers
open Http.Classes

inductive ConnectionHeader.Standard where
  | close
  | keepAlive
  | upgrade
  deriving Inhabited, BEq, Repr, Hashable

inductive ConnectionHeader where
  | standard (val: ConnectionHeader.Standard)
  | custom (value: String)
  deriving Inhabited, BEq, Repr, Hashable

private def connectionStandard : Lean.Data.Trie ConnectionHeader.Standard :=
  Lean.Data.Trie.empty
  |>.insert "close" .close
  |>.insert "keep-alive" .keepAlive
  |>.insert "upgrade" .upgrade

instance : Parseable ConnectionHeader.Standard where
  parse name := connectionStandard.find? name.toLower

instance : Canonical .text ConnectionHeader.Standard where
  repr
    | .close     => "close"
    | .keepAlive => "keep-alive"
    | .upgrade   => "upgrade"

instance : Parseable ConnectionHeader where
  parse name := connectionStandard.find? name
    |>.map (.standard)
    |>.orElse (λ_ => some (ConnectionHeader.custom name))

instance : Canonical .text ConnectionHeader where
  repr
    | .standard s => Canonical.text s
    | .custom v   => v

instance : Header .connection ConnectionHeader where
  parse := Parseable.parse
