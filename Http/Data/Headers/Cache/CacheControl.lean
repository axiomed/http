import Http.Data.Headers.Name
import Http.Data.Mime
import Http.Data.Method
import Lean.Data.Parsec
import Http.Util.Parser

namespace Http.Data.Headers
open Http.Classes Lean.Parsec Http.Util.Parser

/-- Directives for caching

* Reference: https://httpwg.org/specs/rfc9111.html#field.cache-control
-/
structure CacheDirective where
  name: String
  value: Option String

def CacheDirective.parser : Lean.Parsec CacheDirective := do
  let name ← token
  let value ← skipChar '=' *> (quotedString <|> token)
  return { name, value }

instance : Header .cacheControl (Array CacheDirective) where
  parse := Except.toOption ∘ (sepByComma CacheDirective.parser <* eof).run
