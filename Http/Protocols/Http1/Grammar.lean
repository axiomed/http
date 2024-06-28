import Parse
import Parse.DSL

namespace Http.Protocols.Http1

open Parse.DSL

/-! HTTP Message Parser based on https://httpwg.org/specs/rfc9112.html. -/

-- an HTTP/1.1 client MUST NOT preface or follow a request with an extra CRLF.

parser Grammar in Lean where
  def type : u8
  def method : u8
  def statusCode : u16
  def major : u8
  def minor : u8
  def reasonPhrase : span
  def url : span
  def body : span
  def chunkData : span
  def prop : span
  def value : span

  def isCL : u8
  def contentLength : u64
  def chunkLength : u64

  set digit := ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
  set ws := [" " "\x09"]

  set token := [
    " " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/" "0" "1" "2" "3" "4"
    "5" "6" "7" "8" "9" ":" "<" ">" "?" "@" "A" "B" "C" "D" "E" "F" "G" "H" "I"
    "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\" "]" "^"
    "_" "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
    "u" "v" "w" "x" "y" "z" "{" "|" "}" "~"
  ]

  callback endProp
  callback endUrl
  callback endField
  callback endFieldExt
  callback endFieldTrailer
  callback endRequest
  callback endHeaders [ contentLength ]
  callback endRequestLine [ method major minor ]
  callback endResponseLine [ statusCode major minor ]

  -- Defines if its a `request-line` or `status-line` this parser is used for both request and response.
  node statusLine where
    select (read type)
      | 0 => httpVersionStart
      default => method

  -- The start of the `request-line` defined in https://httpwg.org/specs/rfc9112.html#request.line
  node method where
    switch (store method beforeUrl)
      | "HEAD" => 0
      | "GET" => 1
      | "POST" => 2
      | "PUT" => 3
      | "DELETE" => 4
      | "OPTIONS" => 5
      | "CONNECT" => 6
      | "TRACE" => 7
      | "PATCH" => 8
    otherwise (error 23)

    node beforeUrl where
      is " " (start url url)

    -- The start of the `request-target` defined in https://httpwg.org/specs/rfc9112.html#request.target
    node url where
      peek ' ' (end url (call endUrl endUrl))
      any url

    node endUrl where
      is " " httpVersionStart

    node httpVersionStart where
      is "HTTP/" httpVersionMajor

    node httpVersionMajor where
      is "1" (call (loadNum major) httpVersionDot)
      is "2" (call (loadNum major) httpVersionDot)

    node httpVersionDot where
      is "." httpVersionMinor

    node httpVersionMinor where
      is digit (call (loadNum minor) httpHeaderEnd)

    node httpHeaderEnd where
      select (read type)
        | 0 => statusCodeStart
        | 1 => lineAlmostDone
        default => error 0

    node statusCodeStart where
      is " " statusCode1

    node statusCode1 where
      is digit (call (mulAdd decimal statusCode) statusCode2)

    node statusCode2 where
      is digit (call (mulAdd decimal statusCode) statusCode3)

    node statusCode3 where
      is digit (call (mulAdd decimal statusCode) resStatusCode)

    node resStatusCode where
      is " " (start reasonPhrase reasonPhrase)
      peek '\r' lineAlmostDone

    node reasonPhrase where
      peek '\r' (end reasonPhrase lineAlmostDone)
      any reasonPhrase

    node lineAlmostDone where
      is "\r\n" endLine

    node endLine where
      select (read type)
        | 0 => responseLine
        default => requestLine

    node responseLine where
      select endResponseLine
        | 0 => fieldLineStart
        default => error 10

    node requestLine where
      select endRequestLine
        | 0 => fieldLineStart
        default => error 20

    node fieldLineStart where
      peek '\r' endHeaders
      otherwise (start prop fieldLineProp)

    node fieldLineProp where
      peek ':' (end prop (call (callStore endProp isCL) fieldLineColon))
      any fieldLineProp
      otherwise (error 1)

    node fieldLineColon where
      is ":" fieldLineOWS
      otherwise (error 2)

    node fieldLineOWS where
      is " " fieldLineOWS
      otherwise (start value fieldLineValuePre)
      otherwise (error 3)

    node fieldLineValuePre where
      select (read isCL)
        | 0 => fieldLineValue
        | 1 => contentLength
        default => error 10

    node contentLength where
      peek '\r' (end value selectLineEnd)
      is digit (call (mulAdd decimal contentLength) contentLength)
      otherwise (error 5)

    node fieldLineValue where
      peek '\r' (end value selectLineEnd)
      any fieldLineValue

    node endHeaders where
      select endHeaders
        | 0 => endMessage
        | 1 => startChunker
        | 2 => unlimitedBodyStart
        default => error 2

    node selectLineEnd where
      select endField
        | 0 => fieldLineEnd
        default => error 22

    node fieldLineEnd where
      is "\r\n" fieldLineStart

    node endMessage where
      is "\r\n" (start body bodyInfo)

    node bodyInfo where
      consume contentLength (end body theEnd)

    node unlimitedBodyStart where
      is "\r\n" (start body unlimitedBody)

    node unlimitedBody where
      any unlimitedBody

    node startChunker where
      is "\r\n" (call (store chunkLength 0) chunkSize)
      otherwise (error 120)

    node chunk where
      otherwise (call (store chunkLength 0) chunkSize)

    node chunkSize where
      is digit (call (mulAdd hex chunkLength) chunkParseLength)
      otherwise (error 121)

    node chunkParseLength where
      is digit (call (mulAdd hex chunkLength) chunkParseLength)
      otherwise chunkExtension

    node chunkExtension where
      is ";" (start prop chunkExtensionName)
      otherwise chunkData

    node chunkExtensionName where
      is token chunkExtensionName
      otherwise (end prop chunkExtensionColon)

    node chunkExtensionColon where
      is "=" (start value chunkExtensionVal)
      otherwise callFieldExt

    node chunkExtensionVal where
      is token chunkExtensionVal
      is "\"" chunkExtensionValStr
      otherwise (end value callFieldExt)

    node callFieldExt where
      select endFieldExt
        | 0 => chunkExtension
        default => error 22

    node chunkExtensionValStr where
      is "\"" (end value chunkExtension)
      any chunkExtensionValStr

    node chunkData where
      is "\r\n" (start chunkData chunkDataInfo)
      otherwise (error 123)

    node chunkDataInfo where
      consume chunkLength (end chunkData chunkDataEnd)
      otherwise (error 124)

    node chunkDataEnd where
      select (read chunkLength)
        | 0 => trailer
        default => chunkDataSelect

    node chunkDataSelect where
      is "\r\n" chunk
      otherwise (error 125)

    node trailer where
      is "\r\n" theEnd
      otherwise (start prop trailerProp)

    node trailerProp where
      peek ':' (end prop (call (callStore endProp isCL) trailerColon))
      any trailerProp

    node trailerColon where
      is ":" trailerOWS

    node trailerOWS where
      is " " trailerOWS
      otherwise (start value trailerValue)

    node trailerValue where
      peek '\r' (end value trailerFieldEnd)
      any trailerValue

    node trailerFieldEnd where
      is "\r\n" callFieldTrailer

    node callFieldTrailer where
      select endFieldTrailer
        | 0 => trailer
        default => error 22

    node theEnd where
      otherwise (call endRequest (call (store contentLength 0) statusLine))
