namespace Http.Data

/-! Definition of the [Method] inductive type with all the HTTP Methods for requesting data. -/

/-- A method is a verb that describes the action to be performed. -/
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

instance : ToString Method where
  toString
    | Method.get     => "GET"
    | Method.head    => "HEAD"
    | Method.post    => "POST"
    | Method.put     => "PUT"
    | Method.delete  => "DELETE"
    | Method.connect => "CONNECT"
    | Method.options => "OPTIONS"
    | Method.trace   => "TRACE"
    | Method.patch   => "PATCH"

def Method.fromString : String → Option Method
    | "GET"     => some Method.get
    | "HEAD"    => some Method.head
    | "POST"    => some Method.post
    | "PUT"     => some Method.put
    | "DELETE"  => some Method.delete
    | "CONNECT" => some Method.connect
    | "OPTIONS" => some Method.options
    | "TRACE"   => some Method.trace
    | "PATCH"   => some Method.patch
    | _         => none

def Method.fromNumber : Nat → Option Method
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
