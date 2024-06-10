namespace Http.Data

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
  deriving Repr, Inhabited

namespace Method

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

instance : ToString Method where
  toString
    | .get     => "GET"
    | .head    => "HEAD"
    | .post    => "POST"
    | .put     => "PUT"
    | .delete  => "DELETE"
    | .connect => "CONNECT"
    | .options => "OPTIONS"
    | .trace   => "TRACE"
    | .patch   => "PATCH"

def fromString : String → Option Method
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

def fromNumber : Nat → Option Method
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
