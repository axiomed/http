import Http.Util.Parser
import Http.Data.Cookie.Basic
import Lean.Data.AssocList
import Lean.Data.Parsec
import Time

namespace Http.Data.Cookie
open Http.Util.Parser
open Lean.Parsec
open Lean

private def cookieName : Lean.Parsec String := token

private def cookieValue : Lean.Parsec (Bool × String) :=
  (false, ·) <$> manyChars (satisfy (λ c => c ≠ ';' && c ≠ ','))

private def quotedString : Lean.Parsec (Bool × String) :=
  skipChar '"' *> (true, ·) <$> manyChars (satisfy (· ≠ '"')) <* skipChar '"'

-- Attributes

private def expiresAttribute : Lean.Parsec (Time.DateTime .GMT) := do
  let res ← (many1Chars (satisfy (· ≠ ';')))
  match Http.Util.Date.RFC822.parse res with
  | .ok res => return res
  | .error err => fail err

private def sameSiteAttribute : Lean.Parsec SameSite := do
  let value ← token
  match value.toLower with
  | "strict" => pure $ SameSite.strict
  | "lax" => pure $ SameSite.strict
  | "none" => pure $ SameSite.none
  | _ => fail "cannot match value of same-site"

-- TODO: Add 'token' parsr class satisfy instead of this
private def parseAttribute (cookie: Cookie) : Lean.Parsec Cookie := do
  let name ← token
  dbg_trace name
  match name.toLower with
  | "samesite" => return { cookie with sameSite := (← skipChar '=' *> sameSiteAttribute) }
  | "expires" => return { cookie with expires := (← skipChar '=' *> expiresAttribute) }
  | "max-age" => return { cookie with maxAge := (← skipChar '=' *> number) }
  | "domain" => return { cookie with domain := (← skipChar '=' *> token) }
  | "path" => return { cookie with path := (← skipChar '=' *> token) }
  | "secure" => return { cookie with secure := true }
  | "httponly" => return { cookie with httpOnly := true }
  | "partitioned" => return { cookie with partitioned := true }
  | _ => return cookie

private partial def parseAttributes (cookie: Cookie) : Lean.Parsec Cookie := do
  let cookieOpt ← optional (skipString "; " *> parseAttribute cookie)
  match cookieOpt with
  | some cookie => parseAttributes cookie
  | none => return cookie

private def cookieParser : Lean.Parsec Cookie := do
  let name ← cookieName
  skipChar '='
  let (quoted, value) ← quotedString <|> cookieValue
  return (Cookie.new name value quoted)

private def setCookieParser : Lean.Parsec Cookie := do
  let cookie ← cookieParser
  parseAttributes cookie

-- Parse entries

def parseSet (s: String) : Except String Cookie :=
  (setCookieParser <* eof).run s

def parseCookies (s: String) : Except String (Array Cookie) :=
  ((sepBy cookieParser (skipString "; ")) <* eof).run s

def parseCookie (s: String) : Except String Cookie :=
  cookieParser.run s
