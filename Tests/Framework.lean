def reset := "\u001B[0m"
def red := "\u001B[31m"
def green := "\u001B[32m"

def test (message : String) (result : Bool) : IO Bool := do
  if result then
    IO.println (green ++ message ++ reset)
    return true
  else
    IO.println (red ++ message ++ reset)
    return false

def run (results : List (IO Bool)) := do
  let rec go (remaining : List (IO Bool)) (failed : Nat) : IO Nat :=
    match remaining with
    | [] => return failed
    | x :: [] => bind x (fun success => if success then return failed else return failed + 1)
    | x :: xs => bind x (fun success => if success then (go xs failed) else (go xs (failed + 1)))

  do
    let failed <- go results 0
    if failed > 0 then IO.println s!"{failed} tests failed" else IO.println "All tests have passed!"
    if failed > 0 then IO.Process.exit 1

