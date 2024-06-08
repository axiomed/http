import CaseInsensitive
import DHashMap
import Time
import Lean

namespace Http.Data
open Lean

inductive TransferEncoding where
  | chunked
  | custom (value: String)
  deriving BEq

inductive HeaderName.Standard where
  | date
  | transferEncoding
  | contentType
  | connection
  deriving BEq, Hashable, DecidableEq

instance : ToString HeaderName.Standard where
  toString
    | .date => "date"
    | .transferEncoding => "transfer-encoding"
    | .contentType => "content-type"
    | .connection => "connection"

/-- Class definition for Header, which includes methods to parse and stringify values of type `α` -/
class Header (α: Type 0) where
  parse : String → Option α
  stringify: α → String

/-- Instance for custom Headers mainly -/
instance : Header String where
  parse := some
  stringify := id

/-- Instance based on https://datatracker.ietf.org/doc/html/rfc2616#section-3.3.1 -/
instance : Header (Time.DateTime .GMT) where
  /- though they MUST only generate the RFC 1123 format for representing HTTP-date values in header fields. -/
  stringify := RFC822.format

  /- HTTP/1.1 clients and servers that parse the date value MUST accept all three formats
    All HTTP date/time stamps MUST be represented in Greenwich Mean Time (GMT),
  -/
  parse :=
    Except.toOption ∘ Time.Format.choiceParse #[RFC822, RFC850, AscTime]
  where
    AscTime := Time.Format.spec! "EEE MMM d hh:mm:ss YYYY"
    RFC822 := Time.Format.spec! "EEE, DD MMM YYYY hh:mm:ss 'GMT'"
    RFC850 := Time.Format.spec! "EEEE, DD-MMM-YY hh:mm:ss 'GMT'"

/-- Instance for the Transfer Encoding Header -/
instance : Header TransferEncoding where
  parse
    | "chunked" => TransferEncoding.chunked
    | other => TransferEncoding.custom other
  stringify
    | .chunked => "chunked"
    | .custom s => s

def HeaderName.Standard.getType : HeaderName.Standard → Sigma Header
  | .date => ⟨Time.DateTime .GMT, inferInstance⟩
  -- Defined in [https://datatracker.ietf.org/doc/html/rfc2616#section-3.6]
  | .transferEncoding => ⟨TransferEncoding, inferInstance⟩
  -- We should add [https://datatracker.ietf.org/doc/html/rfc2616#section-3.7]?
  | .contentType => ⟨String, inferInstance⟩
  -- Connection parameter
  | .connection => ⟨String, inferInstance⟩

inductive HeaderName
  | standard (standard: HeaderName.Standard)
  | custom (name: String.CI)
  deriving BEq, Hashable, DecidableEq

instance : ToString HeaderName where
  toString
    | .standard std => toString std
    | .custom ci => ci.value

def headerNameTrie : Lean.Data.Trie HeaderName.Standard :=
  Lean.Data.Trie.empty
  |>.insert "date" .date
  |>.insert "transfer-encoding" .transferEncoding

def HeaderName.parse (x: String.CI) : HeaderName :=
  match headerNameTrie.find? x.value with
  | .some x => .standard x
  | .none => .custom x

def HeaderName.getType : HeaderName → Sigma Header
  | .standard std => std.getType
  | .custom _ => ⟨String, Header.String.instance⟩

def HeaderName.canonicalName (name: HeaderName): String :=
  let lower := toString name
  let parts := lower.split (· == '-')
  let parts := parts.map String.capitalize
  String.intercalate "-" parts

instance : Coe String String.CI where
  coe := String.CI.new

instance : Coe String HeaderName where
  coe := HeaderName.parse ∘ String.CI.new

instance : Coe String.CI HeaderName where
  coe := HeaderName.parse

instance : Coe HeaderName.Standard HeaderName where
  coe := .standard

instance : ToString HeaderName where
  toString
    | .standard std => toString std
    | .custom x => x.value
