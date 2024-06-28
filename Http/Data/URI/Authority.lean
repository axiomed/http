import CaseInsensitive
import Http.Classes

namespace Http.Data.Uri
open Http.Classes

/-- TCP number port -/
abbrev Port := UInt16

/--Tthe authority component in a URI provides the necessary information for locating the resource
on the network.

* Reference: https://www.rfc-editor.org/rfc/rfc3986.html#section-3.1
-/
structure Authority where
  userInfo: Option String := none
  host: Option String := none
  port: Option Port := none
  deriving BEq, Repr, Inhabited

instance : Canonical .text Authority where
  repr authority :=
    let userInfo := (authority.userInfo.map (· ++ ":")).getD ""
    let port := (authority.port.map (":" ++ toString ·)).getD ""
    s!"{userInfo}{authority.host.getD ""}{port}"
