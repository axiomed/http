import Http.Data.Headers.Auth.WWWAuthenticate
import Http.Data.Headers.Name

namespace Http.Data.Headers

instance : Header .proxyAuthenticate Challenge where
  parse := Except.toOption ∘ Challenge.parse
