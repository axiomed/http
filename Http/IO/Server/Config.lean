import Http.Data.Uri

namespace Http.IO.Server
open Http.Data

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

/-- Structure to configure a HTTP web server -/
structure Config where
  /-- Server name in the response headers -/
  name: Option String

  /-- Option to add Connection: Closed when it's shutting down -/
  closeOnShutdown: Bool

  /-- If it will use keep alive headers to multiple requests at the same connection-/
  tcpKeepAlive: Bool

  /-- Time in seconds to close the connection after some request is sent -/
  idleTimeout : Nat

  /-- Configurations for parsing a message. -/
  messageConfig : MessageConfig

instance : Inhabited Config where
  default :=
    { name := some "Http.lean"
    , closeOnShutdown := true
    , tcpKeepAlive := true
    , idleTimeout := 1
    , messageConfig := Inhabited.default
    }
