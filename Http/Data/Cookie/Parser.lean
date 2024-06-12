import Http.Util.Parser
import Http.Data.Cookie.Basic
import Lean.Data.AssocList
import Lean.Data.Parsec
import Http.Classes
import Time

namespace Http.Data.Cookie
open Http.Util.Parser
open Http.Classes
open Lean.Parsec
open Lean

/- Reference: https://httpwg.org/specs/rfc6265.html#sane-set-cookie-syntax -/

/-- cookie-name = token -/
private def cookieName : Lean.Parsec String :=
  token

/-- cookie-value = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE ) -/
private def cookieValue : Lean.Parsec (Bool × String)
  :=  (false, ·) <$> cookieOctet
  <|> (true, ·) <$> (skipChar '"' *> manyChars cookieOctetChar <* skipChar '"')

/-- expires-av = "Expires=" sane-cookie-date -/
private def expiresAttribute : Lean.Parsec (Time.DateTime .GMT) := do
  let res ← (many1Chars (satisfy (· ≠ ';')))
  match Http.Util.Date.RFC822.parse res with
  | .ok res => return res
  | .error err => fail err

/- samesite-av = "SameSite" / "SameSite=" samesite-value
   samesite-value = "Strict" / "Lax" -/
private def sameSiteAttribute : Lean.Parsec SameSite := do
  let value ← token
  match value with
  | "Strict" => pure $ SameSite.strict
  | "Lax" => pure $ SameSite.strict
  | "None" => pure $ SameSite.none
  | _ => fail "cannot match value of same-site"

/-- any CHAR except CTLs or ";" -/
private def notSemi : Lean.Parsec String :=
  many1Chars (satisfy (λc => ¬isControl c ∧ isASCII c ∧ c ≠ ';'))

/-- cookie-av = expires-av / max-age-av / domain-av / path-av / secure-av / httponly-av / extension-av -/
private def parseAttribute (cookie: Cookie) : Lean.Parsec Cookie := do
  match (← token) with
  | "SameSite" => return { cookie with sameSite := (← skipChar '=' *> sameSiteAttribute) }
  | "Expires" => return { cookie with expires := (← skipChar '=' *> expiresAttribute) }
  | "Max-Age" => return { cookie with maxAge := (← skipChar '=' *> number) }
  | "Domain" => return { cookie with domain := (← skipChar '=' *> notSemi) }
  | "Path" => return { cookie with path := (← skipChar '=' *> notSemi) }
  | "Secure" => return { cookie with secure := true }
  | "HttpOnly" => return { cookie with httpOnly := true }
  | "Partitioned" => return { cookie with partitioned := true }
  | _ => fail "invalid attribute"

/-- set-cookie-string = cookie-pair *( ";" SP cookie-av ) -/
private partial def parseAttributes (cookie: Cookie) : Lean.Parsec Cookie := do
  let cookieOpt ← optional (skipString "; " *> parseAttribute cookie)
  match cookieOpt with
  | some cookie => parseAttributes cookie
  | none => return cookie

/-- cookie-pair = cookie-name "=" cookie-value -/
private def cookieParser : Lean.Parsec Cookie := do
  let name ← cookieName
  skipChar '='
  let (quoted, value) ← cookieValue
  return (Cookie.new name value quoted)

/-- set-cookie-string = cookie-pair *( ";" SP cookie-av ) -/
private def setCookieParser : Lean.Parsec Cookie := do
  let cookie ← cookieParser
  parseAttributes cookie

/-- Parse entries -/

def parseSet (s: String) : Except String Cookie :=
  (setCookieParser <* eof).run s

def parseCookies (s: String) : Except String (Array Cookie) :=
  ((sepBy cookieParser (skipString "; ")) <* eof).run s

def parseCookie (s: String) : Except String Cookie :=
  cookieParser.run s

-- Instances

instance : Parseable Cookie where
  parse := Except.toOption ∘ parseSet
