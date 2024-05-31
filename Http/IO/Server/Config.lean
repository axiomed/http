/-!
  Http Server configuration.
-/

namespace Http.IO.Server

/-- Structure to configure a HTTP web server -/
structure Config where
  /-- Server name in the response headers -/
  name: Option String

  /-- Option to add Connection: Closed when it's shutting down -/
  closeOnShutdown: Bool

  /-- If it will use keep alive headers to multiple requests at the same connection-/
  tcpKeepAlive: Bool

  /-- Maximum size of request body, the server will reject if exceeds this limit -/
  maxRequestBody: Nat

  /-- Time in seconds to close the connection after some request is sent -/
  idleTimeout : Nat

  /-- Number of headers that a request or response can have -/
  maxHeaders : Nat

instance : Inhabited Config where
  default :=
    { name := some "http.lean"
    , closeOnShutdown := true
    , tcpKeepAlive := true
    , maxRequestBody := 30000000
    , idleTimeout := 1
    , maxHeaders := 20
    }
