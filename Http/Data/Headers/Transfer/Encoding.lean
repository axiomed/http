import Http.Data.Headers.Name
import Http.Classes.Parseable
import Http.Util.Parser
import Lean.Data.Parsec

namespace Http.Data.Headers
open Lean.Parsec
open Http.Util.Parser
open Http.Classes


/-- Names used to indicate the encoding that can be applied to the message content.

* Reference: https://httpwg.org/specs/rfc9112.html#transfer.codings
-/
inductive TransferEncodingType
  | chunked
  | gzip
  | deflate
  | brotli
  | zstd
  | identity
  deriving Repr, BEq

instance : Parseable TransferEncodingType where
  parse name := Lean.Data.Trie.empty
    |>.insert "chunked" .chunked
    |>.insert "gzip" .gzip
    |>.insert "deflate" .deflate
    |>.insert "brotli" .brotli
    |>.insert "zstd" .zstd
    |>.insert "identity" .identity
    |>.find? name

instance : Canonical .text TransferEncodingType where
  repr
    | .chunked  => "chunked"
    | .gzip     => "gzip"
    | .deflate  => "deflate"
    | .brotli   => "brotli"
    | .zstd     => "zstd"
    | .identity => "identity"

structure TransferEncoding where
  type: TransferEncodingType
  params: Array (String × String)
  deriving Repr

def TransferEncoding.isChunked (te: TransferEncoding) : Bool :=
  te.type == .chunked

instance : Canonical .text TransferEncoding where
  repr te :=
    let paramsStr := te.params.map (fun (k, v) => s!"{k}={v}")
    let paramsStr := String.intercalate "; " paramsStr.toList
    if paramsStr.isEmpty then
      Canonical.text te.type
    else
      s!"{Canonical.text te.type}; {paramsStr}"

def TransferEncodingType.parser : Lean.Parsec TransferEncodingType := do
  let scheme ← token
  let scheme := Parseable.parse scheme.toLower

  match scheme with
  | none => fail "invalid transfer encoding type"
  | some scheme => pure scheme

def TransferEncoding.parser : Lean.Parsec TransferEncoding := do
  let type ← TransferEncodingType.parser
  let params ← many do
    ws
    skipChar ';'
    ws
    let name ← token
    skipChar '='
    let value ← token <|> quotedString
    pure (name, value)
  return { type, params }

instance : Header .transferEncoding (Array TransferEncoding) where
  parse := Except.toOption ∘ (sepByComma TransferEncoding.parser <* eof).run
