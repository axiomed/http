-- | A method is a verb that describes the action to be performed.
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

instance : ToString Method where
  toString
    | Method.get => "GET"
    | Method.head => "HEAD"
    | Method.post => "POST"
    | Method.put => "PUT"
    | Method.delete => "DELETE"
    | Method.connect => "CONNECT"
    | Method.options => "OPTIONS"
    | Method.trace => "TRACE"
    | Method.patch => "PATCH"
