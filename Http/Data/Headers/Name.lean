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
  | upgrade
  | keepAlive
  deriving Inhabited, BEq, Repr, Hashable

private def standard : Lean.Data.Trie HeaderName.Standard :=
  Lean.Data.Trie.empty
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
  |>.insert "upgrade" .upgrade
  |>.insert "keep-alive" .keepAlive

instance : Parseable HeaderName.Standard where
  parse name := standard.find? name

instance : Canonical .text HeaderName.Standard where
  repr
    | .accept                  => "Accept"
    | .acceptCharset           => "Accept-Charset"
    | .acceptEncoding          => "Accept-Encoding"
    | .acceptLanguage          => "Accept-Language"
    | .age                     => "Age"
    | .allow                   => "Allow"
    | .authorization           => "Authorization"
    | .cacheControl            => "Cache-Control"
    | .connection              => "Connection"
    | .contentEncoding         => "Content-Encoding"
    | .contentLength           => "Content-Length"
    | .contentLocation         => "Content-Location"
    | .contentType             => "Content-Type"
    | .date                    => "Date"
    | .expires                 => "Expires"
    | .host                    => "Host"
    | .ifMatch                 => "If-Match"
    | .ifModifiedSince         => "If-Modified-Since"
    | .ifNoneMatch             => "If-None-Match"
    | .ifRange                 => "If-Range"
    | .ifUnmodifiedSince       => "If-Unmodified-Since"
    | .proxyAuthenticate       => "Proxy-Authenticate"
    | .proxyAuthorization      => "Proxy-Authorization"
    | .setCookie               => "Set-Cookie"
    | .transferEncoding        => "Transfer-Encoding"
    | .wwwAuthenticate         => "Www-Authenticate"
    | .upgrade                 => "Upgrade"
    | .keepAlive               => "Keep-Alive"

inductive HeaderName where
  | standard (val: HeaderName.Standard)
  | custom (value: String.CI)
  deriving Inhabited, BEq, Repr, Hashable

def canonicalHeaderName (s: String) : String :=
  s.split (· == '-')
  |>.map String.capitalize
  |> String.intercalate "-"

instance : Standard HeaderName HeaderName.Standard where
  custom := HeaderName.custom ∘ String.CI.new
  standard := HeaderName.standard

instance : Canonical .text HeaderName where
  repr
    | .standard std => Canonical.text std
    | .custom str => toString str

instance : Canonical .text HeaderName where
  repr
    | .standard std => Canonical.text std
    | .custom str => Canonical.text (canonicalHeaderName str.value)

class Header (name: HeaderName.Standard) (α: outParam Type) where
  parse : String → Option α

instance : Coe String HeaderName where
  coe str :=
    match Parseable.parse str.toLower with
    | some res => .standard res
    | none => .custom (String.CI.new str)

instance : Coe HeaderName.Standard HeaderName where
  coe := .standard
