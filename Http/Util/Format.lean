namespace Http.Util.Format

def toHexChar (n : Nat) : Char :=
  if n < 10
    then Char.ofNat (n + 48)
    else Char.ofNat (n + 87)

def toHex (n : Nat) : String := Id.run do
  let mut value := n
  let mut result := ""
  if value == 0 then return "0"

  while value > 0 do
    let digit := value % 16
    result := result.push $ toHexChar digit
    value := value / 16

  return result
