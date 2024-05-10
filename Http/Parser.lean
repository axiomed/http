import Parse
import Parse.DSL

namespace Http

open Parse.DSL

/-!
  HTTP Message Parser based on https://httpwg.org/specs/rfc9112.html.
  - We do not replace CR with SP before processing
-/

parser Parser in C where
  def type : u8
  def method : u8
  def statusCode : u16
  def major : u8
  def minor : u8
  def reasonPhrase : span
  def url : span
  def body : span
  def prop : span
  def value : span

  def isCL : u8
  def contentLength : u64

  set digit := ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]
  set ws := [" " "\x09"]

  callback endProp
  callback endUrl
  callback endField
  callback endHeaders
  callback endRequestLine : method major minor

  -- Defines if its a `request-line` or `status-line`, this parser is used for both request and response.
  node statusLine where
    peek 'H' (call (store type 0) httpVersionStart)
    otherwise (call (store type 1) method)

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
    otherwise (error 2)

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
      is digit (call (mulAdd statusCode) statusCode2)

    node statusCode2 where
      is digit (call (mulAdd statusCode) statusCode3)

    node statusCode3 where
      is digit (call (mulAdd statusCode) resStatusCode)

    node resStatusCode where
      is " " (start reasonPhrase reasonPhrase)
      peek '\r' lineAlmostDone

    node reasonPhrase where
      peek '\r' (end reasonPhrase lineAlmostDone)
      any reasonPhrase

    node lineAlmostDone where
      is "\r\n" requestLine

    node requestLine where
      select endRequestLine
        | 0 => fieldLineStart
        default => error 1

    node fieldLineStart where
      peek '\r' endHeaders
      otherwise (start prop fieldLineProp)

    node endHeaders where
      select endHeaders
        | 0 => endMessage
        default => error 1

    node fieldLineProp where
      peek ':' (end prop (call (callStore endProp isCL) fieldLineColon))
      any fieldLineProp

    node fieldLineColon where
      is ":" fieldLineOWS

    node fieldLineOWS where
      is " " fieldLineOWS
      otherwise (start value fieldLineValuePre)

    node fieldLineValuePre where
      select (read isCL)
        | 0 => fieldLineValue
        | 1 => contentLength
        default => error 0

    node contentLength where
      peek '\r' (end value selectLineEnd)
      is digit (call (mulAdd contentLength) contentLength)

    node fieldLineValue where
      peek '\r' (end value selectLineEnd)
      any fieldLineValue

    node selectLineEnd where
      select endField
        | 0 => fieldLineEnd
        default => error 1

    node fieldLineEnd where
      is "\r\n" fieldLineStart

    node endMessage where
      is "\r\n" (start body body)

    node body where
      otherwise (consume contentLength (end body theEnd))

    node theEnd where
      otherwise (call (store contentLength 0) statusLine)
