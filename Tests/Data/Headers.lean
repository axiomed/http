import Http.Data.Headers

def testIsEmpty :=
  Headers.empty.isEmpty

def tests : List Bool := [
  testIsEmpty
]
