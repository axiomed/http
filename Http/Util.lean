import Http.Util.Date
import Http.Util.Elab
import Http.Util.Format
import Http.Util.Parser
import LibUV

def IO.toUVIO (act: IO α) : UV.IO α := IO.toEIO (λx => UV.Error.user x.toString) act

def String.fromAscii (arr: ByteArray) : String := Id.run $ do
  let mut s := ""
  for byte in arr do s := s.push $ Char.ofNat byte.toNat
  return s
