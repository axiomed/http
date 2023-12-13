-- | A version of the HTTP protocol.
structure Version where
  major : Nat
  minor : Nat
  deriving Repr

instance : ToString Version where
  toString v := "HTTP/" ++ toString v.major ++ "." ++ toString v.minor

-- | https://datatracker.ietf.org/doc/html/rfc1945
def Version.OneZero := Version.mk 1 0

-- | https://datatracker.ietf.org/doc/html/rfc2616
def Version.OneOne := Version.mk 1 1
