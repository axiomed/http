import Http.Data.Headers

namespace Http.Protocols.Http1.Data

inductive SizeMarker
  | length (nat: Nat)
  | chunked
  | none
