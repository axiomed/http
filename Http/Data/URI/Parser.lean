import Http.Data.URI.Grammar
import Http.Data.URI

namespace Http.Data.URI

structure URI.State where
  authority : Option String
  path      : Option String
  query     : Option String
  fragment  : Option String
  scheme    : Option String
  port      : Option String
  deriving Inhabited

/-! Defines an incremental parser handler using the `Grammar` module. This parser is designed to handle
    URI components incrementally.
-/
abbrev Parser := Grammar.Data URI.State

private def setField (func: String → URI.State → URI.State) : Nat → Nat → ByteArray → URI.State → IO (URI.State × Nat) :=
  fun st en bs acc => pure (func (String.fromAscii $ bs.extract st en) acc, 0)

/-- Creates a new parser structure. This parser uses various field-setting functions to update the
URI components. -/
def Parser.create : Parser :=
    Grammar.create
      (onPath := setField (λval acc => {acc with path := appendOr acc.path val }))
      (onPort := setField (λval acc => {acc with port := appendOr acc.port val }))
      (onSchema := setField (λval acc => {acc with scheme := appendOr acc.scheme val }))
      (onHost := setField (λval acc => {acc with authority := appendOr acc.authority val }))
      (onQuery := setField (λval acc => {acc with query := appendOr acc.query val }))
      (onFragment := setField (λval acc => {acc with fragment := appendOr acc.fragment val }))
      Inhabited.default
  where
    appendOr (data: Option String) (str: String) : Option String :=
      match data with
      | some res => some $ res.append str
      | none => some str

/-- Feeds data into the parser. This function takes a parser and a ByteArray, and processes the
data to update the parser state incrementally. -/
def Parser.feed (parser: Parser) (data: ByteArray) : IO Parser :=
  Grammar.parse parser data

/-- Retrieves the parsed URI data from the parser. This function extracts the accumulated URI
information from the parser. -/
def Parser.data (parser: Parser) : Except Nat URI :=
  if parser.error ≠ 0 then
    .error parser.error
  else
    let state := Grammar.Data.info parser
    let port := state.port.map (· |> String.toNat! |>.toUInt16)
    let (userinfo, host) :=
      match state.authority.getD "" |>.split (· = '@') with
      | [userinfo, host] => (some userinfo, some host)
      | [host] => (none, some host)
      | [] => (none, none)
      | _ => panic "impossible uri"
    let authority := Authority.mk userinfo host port
    .ok (URI.mk authority state.path state.query state.fragment)
