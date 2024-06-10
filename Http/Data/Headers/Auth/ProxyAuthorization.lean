import Http.Data.Headers.Auth.Authorization
import Http.Data.Headers.Name

namespace Http.Data.Headers

instance : Header .proxyAuthorization Authorization where
  parse := Except.toOption ∘ Authorization.parse
