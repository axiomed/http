import «Tests».Data.Headers

def main : IO Unit :=
  let all := List.all tests (fun res => res)
  IO.println all
