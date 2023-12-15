import «Tests».Data.Headers
import «Tests».Framework

def main : IO Unit := do
  let tests := [
    test "Headers.isEmpty returns true on Request.empty" testIsEmpty
  ]
  
  run tests
