import Http.Data.Headers.Name
import Http.Classes.FromString
import Http.Util.Parser
import Lean.Data.Parsec

namespace Http.Data.Headers
open Lean.Parsec
open Http.Util.Parser
open Http.Classes

inductive AuthorizationScheme
  | basic
  | bearer
  | digest
  | hoba
  | mutual
  | negotiate
  | oauth
  | scramsha1
  | scrumsha256
  | vapid
  deriving Repr

instance : Canonical AuthorizationScheme where
  repr
    | .basic => "Basic"
    | .bearer => "Bearer"
    | .digest => "Digest"
    | .hoba => "HOBA"
    | .mutual => "Mutual"
    | .negotiate => "Negotiate"
    | .oauth => "OAuth"
    | .scramsha1 => "SCRAM-SHA-1"
    | .scrumsha256 => "SCRAM-SHA-256"
    | .vapid => "vapid"

instance : FromString AuthorizationScheme where
  trie := Lean.Data.Trie.empty
      |>.insert "basic" .basic
      |>.insert "bearer" .bearer
      |>.insert "digest" .digest
      |>.insert "hoba" .hoba
      |>.insert "mutual" .mutual
      |>.insert "negotiate" .negotiate
      |>.insert "oauth" .oauth
      |>.insert "scram-sha- 1" .scramsha1
      |>.insert "scrum-sha-256" .scrumsha256
      |>.insert "vapid" .vapid

def AuthorizationScheme.parser : Lean.Parsec AuthorizationScheme := do
  let scheme ← token
  let scheme := FromString.fromString (α := AuthorizationScheme) scheme.toLower

  match scheme with
  | none => fail "invalid scheme"
  | some scheme => pure scheme

/-- Allows a user-agent to authenticate in the origin server

* Reference: https://datatracker.ietf.org/doc/html/rfc7235#section-4.2
-/
structure Authorization where
  scheme : AuthorizationScheme
  data : String
  deriving Repr

instance : Canonical Authorization where
  repr x := s!"{Canonical.repr x.scheme} {x.data}"

def Authorization.parser : Lean.Parsec Authorization := do
  let scheme ← AuthorizationScheme.parser
  skipChar ' '
  let data ← ws *> token
  return { scheme, data }

def Authorization.parse : String → Except String Authorization :=
  (Authorization.parser <* eof).run

instance : Header .authorization Authorization where
  parse := Except.toOption ∘ Authorization.parse
