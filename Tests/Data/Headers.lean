import Soda
import Soda.Grape
import Soda.Grape.Text

import Http.Data.Headers
import «Tests».Framework


def headersTests: List (IO Bool) := [
  test "Headers.isEmpty returns true on Request.empty"
    Headers.empty.isEmpty,

  test "Headers.isEmpty returns true on Request.empty"
    (match (Grape.run Headers.headerParser ("foo: bar".toSlice)) with
    | Result.done result _ => result == ("foo", "bar")
    | _ => false)
]
