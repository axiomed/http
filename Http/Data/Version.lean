namespace Http.Data

/-- The 'Version' structure represents an HTTP version with a major and minor number. It includes
several standard versions of the HTTP protocol, such as HTTP/0.9, HTTP/1.0,  HTTP/1.1, HTTP/2.0, and
HTTP/3.0.

* Reference: https://httpwg.org/specs/rfc9110.html#protocol.version
-/
inductive Version
  | v09
  | v10
  | v11
  | v20
  | v30


instance : ToString Version where
  toString
    | .v09 => "HTTP/0.9"
    | .v10 => "HTTP/1.0"
    | .v11 => "HTTP/1.1"
    | .v20 => "HTTP/2.0"
    | .v30 => "HTTP/3.0"

-- The default version is defined as the HTTP/1.1 version because it's one of the most used versions
-- in 2024.
instance : Inhabited Version where
  default := Version.v11

def Version.fromNumber : Nat → Nat → Option Version
  | 0, 9 => some Version.v09
  | 1, 0 => some Version.v10
  | 1, 1 => some Version.v11
  | 2, 0 => some Version.v20
  | 3, 0 => some Version.v30
  | _, _ => none
