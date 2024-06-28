import Http.Classes

namespace Http.Data
open Http.Classes

/-- A method is a verb that describes the action to be performed.

* Reference: https://httpwg.org/specs/rfc9110.html#methods
-/
inductive Method where
  | get
  | head
  | post
  | put
  | delete
  | connect
  | options
  | trace
  | patch
  deriving Repr, Inhabited, BEq

namespace Method

instance : Canonical .text Method where
  repr
    | .get     => "GET"
    | .head    => "HEAD"
    | .post    => "POST"
    | .put     => "PUT"
    | .delete  => "DELETE"
    | .connect => "CONNECT"
    | .options => "OPTIONS"
    | .trace   => "TRACE"
    | .patch   => "PATCH"

instance : Parseable Method where
  parse
    | "GET"     => some .get
    | "HEAD"    => some .head
    | "POST"    => some .post
    | "PUT"     => some .put
    | "DELETE"  => some .delete
    | "CONNECT" => some .connect
    | "OPTIONS" => some .options
    | "TRACE"   => some .trace
    | "PATCH"   => some .patch
    | _         => none

/-- Transform arbitrary Nat into a Method, it's useful for the parser function that returns an Int. -/
def ofNat : Nat → Option Method
    | 0 => some .head
    | 1 => some .get
    | 2 => some .post
    | 3 => some .put
    | 4 => some .delete
    | 5 => some .options
    | 6 => some .connect
    | 7 => some .trace
    | 8 => some .patch
    | _ => none

/-- Request methods are considered safe if their defined semantics are essentially read-only

* Reference: https://httpwg.org/specs/rfc9110.html#metho  d.properties
-/
def isSafe : Method → Prop
  | .get | .head | .options | .trace => True
  | _ => False

/-- A request method is considered idempotent if the intended effect on the server of multiple
identical requests with that method is the same as the effect for a single such request.

* Reference: https://httpwg.org/specs/rfc9110.html#idempotent.methods
-/
def isIdempotent : Method → Prop
  | .get | .head | .options | .trace => True
  | .put | .delete => True
  | _ => False

/-- Checks if the given method allows a request body. GET and HEAD methods do not typically allow
request bodies.

* Reference: https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods
-/
def allowsRequestBody : Method → Bool
  | .get | .head => False
  | _ => True
