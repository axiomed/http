import Soda
import Soda.Data.ByteSlice
import Soda.Grape
import Soda.Grape.Text

import Http.Data.Headers
import «Tests».Framework

-- A bit non-sense instance
instance [BEq a] : BEq (Result (a × a)) where
  beq
    | Result.done i is, Result.done j js => i == j && is.toASCIIString == js.toASCIIString
    | Result.error i is, Result.error j js => i == j && is == js
    | Result.cont _, Result.cont _ => false
    | _, _ => false

def headersTests : List Assertion := [
  Assertion.make "Headers.isEmpty returns true on Request.empty" true Headers.empty.isEmpty,

  Assertion.make "Headers.isEmpty returns true on Request.empty" (Result.done ("foo", "bar") default) (Grape.run Headers.headerParser ("foo: bar".toSlice))
]
