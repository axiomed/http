import Parse
import Parse.DSL

namespace Http

open Parse.DSL

/-!
  HTTP Parser based on https://httpwg.org/specs/rfc9112.html
-/

parser Parser where
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
  callback endRequestLine : method major minor

  node statusLine where
    peek 'H' (call (store type 0) httpVersionStart)
    otherwise (call (store type 1) method)

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
      is " " beforeUrl
      otherwise (start url url)

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
      is "\r\n" (call endRequestLine fieldLineStart)

    node fieldLineStart where
      peek '\r' endMessage
      otherwise (start prop fieldLineProp)

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
      peek '\r' (end value fieldLineEnd)
      is digit (call (mulAdd contentLength) contentLength)

    node fieldLineValue where
      peek '\r' (end value (call endField fieldLineEnd))
      any fieldLineValue

    node fieldLineEnd where
      is "\r\n" fieldLineStart

    node endMessage where
      is "\r\n" (start body body)

    node body where
      otherwise (consume contentLength (end body theEnd))

    node theEnd where
      otherwise (call (store contentLength 0) statusLine)
