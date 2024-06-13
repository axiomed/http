namespace Http

/-! Http Server configuration. -/

structure MessageConfig where

  /-- Maximum size of request body, the server will reject if exceeds this limit -/
  maxRequestBody: Option Nat

  /-- Number of headers that a request or response can have -/
  maxHeaders : Nat

  /-- Maximum size of a header value and name -/
  maxHeaderSize : Nat

  /-- Maximum size of a URI -/
  maxURISize : Nat

instance : Inhabited MessageConfig where
  default :=
    { maxRequestBody := none
    , maxHeaders := 20
    , maxURISize := 8175
    , maxHeaderSize := 8175
    }
