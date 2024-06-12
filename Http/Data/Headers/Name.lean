import Http.Classes.Standard
import Http.Classes.Canonical
import Http.Classes.Parseable
import CaseInsensitive

namespace Http.Data.Headers
open Http.Classes

inductive HeaderName.Standard where
  | accept
  | acceptCharset
  | acceptEncoding
  | acceptLanguage
  | age
  | allow
  | authorization
  | cacheControl
  | connection
  | contentEncoding
  | contentLength
  | contentLocation
  | contentType
  | date
  | expires
  | host
  | ifMatch
  | ifModifiedSince
  | ifNoneMatch
  | ifRange
  | ifUnmodifiedSince
  | proxyAuthenticate
  | proxyAuthorization
  | setCookie
  | transferEncoding
  | wwwAuthenticate
  deriving Inhabited, BEq, Repr, Hashable

instance : Parseable HeaderName.Standard where
  parse name := Lean.Data.Trie.empty
    |>.insert "accept" .accept
    |>.insert "accept-charset" .acceptCharset
    |>.insert "accept-encoding" .acceptEncoding
    |>.insert "accept-language" .acceptLanguage
    |>.insert "age" .age
    |>.insert "allow" .allow
    |>.insert "authorization" .authorization
    |>.insert "cache-control" .cacheControl
    |>.insert "connection" .connection
    |>.insert "content-encoding" .contentEncoding
    |>.insert "content-length" .contentLength
    |>.insert "content-location" .contentLocation
    |>.insert "content-type" .contentType
    |>.insert "date" .date
    |>.insert "expires" .expires
    |>.insert "host" .host
    |>.insert "if-match" .ifMatch
    |>.insert "if-modified-since" .ifModifiedSince
    |>.insert "if-none-match" .ifNoneMatch
    |>.insert "if-range" .ifRange
    |>.insert "if-unmodified-since" .ifUnmodifiedSince
    |>.insert "proxy-authenticate" .proxyAuthenticate
    |>.insert "proxy-authorization" .proxyAuthorization
    |>.insert "set-cookie" .setCookie
    |>.insert "transfer-encoding" .transferEncoding
    |>.insert "www-authenticate" .wwwAuthenticate
    |>.find? name

instance : ToString HeaderName.Standard where
  toString
    | .accept                  => "accept"
    | .acceptCharset           => "accept-charset"
    | .acceptEncoding          => "accept-encoding"
    | .acceptLanguage          => "accept-language"
    | .age                     => "age"
    | .allow                   => "allow"
    | .authorization           => "authorization"
    | .cacheControl            => "cache-control"
    | .connection              => "connection"
    | .contentEncoding         => "content-encoding"
    | .contentLength           => "content-length"
    | .contentLocation         => "content-location"
    | .contentType             => "content-type"
    | .date                    => "date"
    | .expires                 => "expires"
    | .host                    => "host"
    | .ifMatch                 => "if-match"
    | .ifModifiedSince         => "if-modified-since"
    | .ifNoneMatch             => "if-none-match"
    | .ifRange                 => "if-range"
    | .ifUnmodifiedSince       => "if-unmodified-since"
    | .proxyAuthenticate       => "proxy-authenticate"
    | .proxyAuthorization      => "proxy-authorization"
    | .setCookie               => "set-cookie"
    | .transferEncoding        => "transfer-encoding"
    | .wwwAuthenticate         => "www-authenticate"

inductive HeaderName where
  | standard (val: HeaderName.Standard)
  | custom (value: String.CI)
  deriving Inhabited, BEq, Repr, Hashable

instance : Standard HeaderName HeaderName.Standard where
  custom := HeaderName.custom ∘ String.CI.new
  standard := HeaderName.standard

instance : ToString HeaderName where
  toString
    | .standard std => toString std
    | .custom str => toString str

instance : Canonical .text HeaderName where
  repr name :=
    let lower := toString name
    let parts := lower.split (· == '-')
    let parts := parts.map String.capitalize
    String.intercalate "-" parts

class Header (name: HeaderName.Standard) (α: outParam Type) where
  parse : String → Option α

class HeaderVal (name: HeaderName) (α: outParam Type) where
  parse : String → Option α

instance : HeaderVal (.custom v) String where
  parse x := some x

instance [i : Header r α] : HeaderVal (.standard r) α where
  parse := i.parse

instance : Coe String HeaderName where
  coe str :=
    match Parseable.parse str.toLower with
    | some res => .standard res
    | none => .custom (String.CI.new str)

instance : Coe HeaderName.Standard HeaderName where
  coe := .standard
