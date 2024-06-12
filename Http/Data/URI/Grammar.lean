import Parse
import Parse.DSL

namespace Http.Data.Uri

/-! This code defines an incremental parser for parsing different components of a URI. -/

open Parse.DSL

parser Grammar in Lean where
  def path : span
  def port : span
  def schema : span
  def host : span
  def query : span
  def fragment : span

  set digit := ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]

  set alpha :=
    ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
     "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
     "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
     "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" ]

  set userinfo_chars :=
    ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
     "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
     "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
     "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
     "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-" "_" "."
     "!" "~" "*" "\"" "(" ")" "%" ";" "&" "=" "+" "$"
     "," ]

  set url_char :=
    ["!" "\"" "$" "%" "&" "\'" "(" ")" "*" "+" "," "-" "."
     "/" ":" ";" "<" "=" ">" "@" "[" "\\" "]" "^" "_" "`"
     "{" "|" "}" "~" "A" "B" "C" "D" "E" "F" "G" "H" "I"
     "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V"
     "W" "X" "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i"
     "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
     "w" "x" "y" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]

  node beginning where
    peek ['/' '*'] (start path path)
    peek alpha (start schema schema)

  node schema where
    is alpha schema
    peek ':' (end schema schemaDelim)
    otherwise (error 1)

  node schemaDelim where
    is "://" (start host server)

  node server where
    is "?" (end host queryStart)
    peek '/' (end host (start path path))
    peek ':' beforePort
    is userinfo_chars server
    is ["[" "]"] server
    is "@" serverWithAt

  node beforePort where
    is ":" (start port port)

  node port where
    is digit port
    otherwise (end port (end host afterPort))

  node afterPort where
    is "?" queryStart
    is "/" (start path path)

  node serverWithAt where
    is "?" (end host queryStart)
    peek '/' (end host (start path path))
    peek ':' (start port port)
    is userinfo_chars serverWithAt
    is ["[" "]"] serverWithAt
    is "@" (error 3)

  node queryStart where
    otherwise (start query query)

  node path where
    is url_char path
    otherwise (end path queryOrFragment)

  node queryOrFragment where
    is "?" (start query query)
    is "#" (start fragment startFragment)

  node query where
    is url_char query
    is "?" query
    peek '#' (end query eatFragStart)

  node eatFragStart where
    is "#" (start fragment fragment)

  node fragment where
    is url_char fragment
    is ["?" "#"] fragment

  node startFragment where
    otherwise (start fragment fragment)
